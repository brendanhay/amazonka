{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListIncomingTypedLinks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
--
module Network.AWS.CloudDirectory.ListIncomingTypedLinks
    (
    -- * Creating a Request
      listIncomingTypedLinks
    , ListIncomingTypedLinks
    -- * Request Lenses
    , litlFilterAttributeRanges
    , litlConsistencyLevel
    , litlNextToken
    , litlFilterTypedLink
    , litlMaxResults
    , litlDirectoryARN
    , litlObjectReference

    -- * Destructuring the Response
    , listIncomingTypedLinksResponse
    , ListIncomingTypedLinksResponse
    -- * Response Lenses
    , litlrsLinkSpecifiers
    , litlrsNextToken
    , litlrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listIncomingTypedLinks' smart constructor.
data ListIncomingTypedLinks = ListIncomingTypedLinks'
  { _litlFilterAttributeRanges :: !(Maybe [TypedLinkAttributeRange])
  , _litlConsistencyLevel      :: !(Maybe ConsistencyLevel)
  , _litlNextToken             :: !(Maybe Text)
  , _litlFilterTypedLink       :: !(Maybe TypedLinkSchemaAndFacetName)
  , _litlMaxResults            :: !(Maybe Nat)
  , _litlDirectoryARN          :: !Text
  , _litlObjectReference       :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIncomingTypedLinks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'litlFilterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
--
-- * 'litlConsistencyLevel' - The consistency level to execute the request at.
--
-- * 'litlNextToken' - The pagination token.
--
-- * 'litlFilterTypedLink' - Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls.
--
-- * 'litlMaxResults' - The maximum number of results to retrieve.
--
-- * 'litlDirectoryARN' - The Amazon Resource Name (ARN) of the directory where you want to list the typed links.
--
-- * 'litlObjectReference' - Reference that identifies the object whose attributes will be listed.
listIncomingTypedLinks
    :: Text -- ^ 'litlDirectoryARN'
    -> ObjectReference -- ^ 'litlObjectReference'
    -> ListIncomingTypedLinks
listIncomingTypedLinks pDirectoryARN_ pObjectReference_ =
  ListIncomingTypedLinks'
    { _litlFilterAttributeRanges = Nothing
    , _litlConsistencyLevel = Nothing
    , _litlNextToken = Nothing
    , _litlFilterTypedLink = Nothing
    , _litlMaxResults = Nothing
    , _litlDirectoryARN = pDirectoryARN_
    , _litlObjectReference = pObjectReference_
    }


-- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
litlFilterAttributeRanges :: Lens' ListIncomingTypedLinks [TypedLinkAttributeRange]
litlFilterAttributeRanges = lens _litlFilterAttributeRanges (\ s a -> s{_litlFilterAttributeRanges = a}) . _Default . _Coerce

-- | The consistency level to execute the request at.
litlConsistencyLevel :: Lens' ListIncomingTypedLinks (Maybe ConsistencyLevel)
litlConsistencyLevel = lens _litlConsistencyLevel (\ s a -> s{_litlConsistencyLevel = a})

-- | The pagination token.
litlNextToken :: Lens' ListIncomingTypedLinks (Maybe Text)
litlNextToken = lens _litlNextToken (\ s a -> s{_litlNextToken = a})

-- | Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls.
litlFilterTypedLink :: Lens' ListIncomingTypedLinks (Maybe TypedLinkSchemaAndFacetName)
litlFilterTypedLink = lens _litlFilterTypedLink (\ s a -> s{_litlFilterTypedLink = a})

-- | The maximum number of results to retrieve.
litlMaxResults :: Lens' ListIncomingTypedLinks (Maybe Natural)
litlMaxResults = lens _litlMaxResults (\ s a -> s{_litlMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the directory where you want to list the typed links.
litlDirectoryARN :: Lens' ListIncomingTypedLinks Text
litlDirectoryARN = lens _litlDirectoryARN (\ s a -> s{_litlDirectoryARN = a})

-- | Reference that identifies the object whose attributes will be listed.
litlObjectReference :: Lens' ListIncomingTypedLinks ObjectReference
litlObjectReference = lens _litlObjectReference (\ s a -> s{_litlObjectReference = a})

instance AWSRequest ListIncomingTypedLinks where
        type Rs ListIncomingTypedLinks =
             ListIncomingTypedLinksResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListIncomingTypedLinksResponse' <$>
                   (x .?> "LinkSpecifiers" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListIncomingTypedLinks where

instance NFData ListIncomingTypedLinks where

instance ToHeaders ListIncomingTypedLinks where
        toHeaders ListIncomingTypedLinks'{..}
          = mconcat
              ["x-amz-data-partition" =# _litlDirectoryARN]

instance ToJSON ListIncomingTypedLinks where
        toJSON ListIncomingTypedLinks'{..}
          = object
              (catMaybes
                 [("FilterAttributeRanges" .=) <$>
                    _litlFilterAttributeRanges,
                  ("ConsistencyLevel" .=) <$> _litlConsistencyLevel,
                  ("NextToken" .=) <$> _litlNextToken,
                  ("FilterTypedLink" .=) <$> _litlFilterTypedLink,
                  ("MaxResults" .=) <$> _litlMaxResults,
                  Just ("ObjectReference" .= _litlObjectReference)])

instance ToPath ListIncomingTypedLinks where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/typedlink/incoming"

instance ToQuery ListIncomingTypedLinks where
        toQuery = const mempty

-- | /See:/ 'listIncomingTypedLinksResponse' smart constructor.
data ListIncomingTypedLinksResponse = ListIncomingTypedLinksResponse'
  { _litlrsLinkSpecifiers :: !(Maybe [TypedLinkSpecifier])
  , _litlrsNextToken      :: !(Maybe Text)
  , _litlrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIncomingTypedLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'litlrsLinkSpecifiers' - Returns one or more typed link specifiers as output.
--
-- * 'litlrsNextToken' - The pagination token.
--
-- * 'litlrsResponseStatus' - -- | The response status code.
listIncomingTypedLinksResponse
    :: Int -- ^ 'litlrsResponseStatus'
    -> ListIncomingTypedLinksResponse
listIncomingTypedLinksResponse pResponseStatus_ =
  ListIncomingTypedLinksResponse'
    { _litlrsLinkSpecifiers = Nothing
    , _litlrsNextToken = Nothing
    , _litlrsResponseStatus = pResponseStatus_
    }


-- | Returns one or more typed link specifiers as output.
litlrsLinkSpecifiers :: Lens' ListIncomingTypedLinksResponse [TypedLinkSpecifier]
litlrsLinkSpecifiers = lens _litlrsLinkSpecifiers (\ s a -> s{_litlrsLinkSpecifiers = a}) . _Default . _Coerce

-- | The pagination token.
litlrsNextToken :: Lens' ListIncomingTypedLinksResponse (Maybe Text)
litlrsNextToken = lens _litlrsNextToken (\ s a -> s{_litlrsNextToken = a})

-- | -- | The response status code.
litlrsResponseStatus :: Lens' ListIncomingTypedLinksResponse Int
litlrsResponseStatus = lens _litlrsResponseStatus (\ s a -> s{_litlrsResponseStatus = a})

instance NFData ListIncomingTypedLinksResponse where
