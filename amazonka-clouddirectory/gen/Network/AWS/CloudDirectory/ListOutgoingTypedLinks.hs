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
-- Module      : Network.AWS.CloudDirectory.ListOutgoingTypedLinks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
--
module Network.AWS.CloudDirectory.ListOutgoingTypedLinks
    (
    -- * Creating a Request
      listOutgoingTypedLinks
    , ListOutgoingTypedLinks
    -- * Request Lenses
    , lotlFilterAttributeRanges
    , lotlConsistencyLevel
    , lotlNextToken
    , lotlFilterTypedLink
    , lotlMaxResults
    , lotlDirectoryARN
    , lotlObjectReference

    -- * Destructuring the Response
    , listOutgoingTypedLinksResponse
    , ListOutgoingTypedLinksResponse
    -- * Response Lenses
    , lotlrsTypedLinkSpecifiers
    , lotlrsNextToken
    , lotlrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listOutgoingTypedLinks' smart constructor.
data ListOutgoingTypedLinks = ListOutgoingTypedLinks'
  { _lotlFilterAttributeRanges :: !(Maybe [TypedLinkAttributeRange])
  , _lotlConsistencyLevel      :: !(Maybe ConsistencyLevel)
  , _lotlNextToken             :: !(Maybe Text)
  , _lotlFilterTypedLink       :: !(Maybe TypedLinkSchemaAndFacetName)
  , _lotlMaxResults            :: !(Maybe Nat)
  , _lotlDirectoryARN          :: !Text
  , _lotlObjectReference       :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOutgoingTypedLinks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lotlFilterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
--
-- * 'lotlConsistencyLevel' - The consistency level to execute the request at.
--
-- * 'lotlNextToken' - The pagination token.
--
-- * 'lotlFilterTypedLink' - Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
--
-- * 'lotlMaxResults' - The maximum number of results to retrieve.
--
-- * 'lotlDirectoryARN' - The Amazon Resource Name (ARN) of the directory where you want to list the typed links.
--
-- * 'lotlObjectReference' - A reference that identifies the object whose attributes will be listed.
listOutgoingTypedLinks
    :: Text -- ^ 'lotlDirectoryARN'
    -> ObjectReference -- ^ 'lotlObjectReference'
    -> ListOutgoingTypedLinks
listOutgoingTypedLinks pDirectoryARN_ pObjectReference_ =
  ListOutgoingTypedLinks'
    { _lotlFilterAttributeRanges = Nothing
    , _lotlConsistencyLevel = Nothing
    , _lotlNextToken = Nothing
    , _lotlFilterTypedLink = Nothing
    , _lotlMaxResults = Nothing
    , _lotlDirectoryARN = pDirectoryARN_
    , _lotlObjectReference = pObjectReference_
    }


-- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
lotlFilterAttributeRanges :: Lens' ListOutgoingTypedLinks [TypedLinkAttributeRange]
lotlFilterAttributeRanges = lens _lotlFilterAttributeRanges (\ s a -> s{_lotlFilterAttributeRanges = a}) . _Default . _Coerce

-- | The consistency level to execute the request at.
lotlConsistencyLevel :: Lens' ListOutgoingTypedLinks (Maybe ConsistencyLevel)
lotlConsistencyLevel = lens _lotlConsistencyLevel (\ s a -> s{_lotlConsistencyLevel = a})

-- | The pagination token.
lotlNextToken :: Lens' ListOutgoingTypedLinks (Maybe Text)
lotlNextToken = lens _lotlNextToken (\ s a -> s{_lotlNextToken = a})

-- | Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
lotlFilterTypedLink :: Lens' ListOutgoingTypedLinks (Maybe TypedLinkSchemaAndFacetName)
lotlFilterTypedLink = lens _lotlFilterTypedLink (\ s a -> s{_lotlFilterTypedLink = a})

-- | The maximum number of results to retrieve.
lotlMaxResults :: Lens' ListOutgoingTypedLinks (Maybe Natural)
lotlMaxResults = lens _lotlMaxResults (\ s a -> s{_lotlMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the directory where you want to list the typed links.
lotlDirectoryARN :: Lens' ListOutgoingTypedLinks Text
lotlDirectoryARN = lens _lotlDirectoryARN (\ s a -> s{_lotlDirectoryARN = a})

-- | A reference that identifies the object whose attributes will be listed.
lotlObjectReference :: Lens' ListOutgoingTypedLinks ObjectReference
lotlObjectReference = lens _lotlObjectReference (\ s a -> s{_lotlObjectReference = a})

instance AWSRequest ListOutgoingTypedLinks where
        type Rs ListOutgoingTypedLinks =
             ListOutgoingTypedLinksResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListOutgoingTypedLinksResponse' <$>
                   (x .?> "TypedLinkSpecifiers" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListOutgoingTypedLinks where

instance NFData ListOutgoingTypedLinks where

instance ToHeaders ListOutgoingTypedLinks where
        toHeaders ListOutgoingTypedLinks'{..}
          = mconcat
              ["x-amz-data-partition" =# _lotlDirectoryARN]

instance ToJSON ListOutgoingTypedLinks where
        toJSON ListOutgoingTypedLinks'{..}
          = object
              (catMaybes
                 [("FilterAttributeRanges" .=) <$>
                    _lotlFilterAttributeRanges,
                  ("ConsistencyLevel" .=) <$> _lotlConsistencyLevel,
                  ("NextToken" .=) <$> _lotlNextToken,
                  ("FilterTypedLink" .=) <$> _lotlFilterTypedLink,
                  ("MaxResults" .=) <$> _lotlMaxResults,
                  Just ("ObjectReference" .= _lotlObjectReference)])

instance ToPath ListOutgoingTypedLinks where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/typedlink/outgoing"

instance ToQuery ListOutgoingTypedLinks where
        toQuery = const mempty

-- | /See:/ 'listOutgoingTypedLinksResponse' smart constructor.
data ListOutgoingTypedLinksResponse = ListOutgoingTypedLinksResponse'
  { _lotlrsTypedLinkSpecifiers :: !(Maybe [TypedLinkSpecifier])
  , _lotlrsNextToken           :: !(Maybe Text)
  , _lotlrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOutgoingTypedLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lotlrsTypedLinkSpecifiers' - Returns a typed link specifier as output.
--
-- * 'lotlrsNextToken' - The pagination token.
--
-- * 'lotlrsResponseStatus' - -- | The response status code.
listOutgoingTypedLinksResponse
    :: Int -- ^ 'lotlrsResponseStatus'
    -> ListOutgoingTypedLinksResponse
listOutgoingTypedLinksResponse pResponseStatus_ =
  ListOutgoingTypedLinksResponse'
    { _lotlrsTypedLinkSpecifiers = Nothing
    , _lotlrsNextToken = Nothing
    , _lotlrsResponseStatus = pResponseStatus_
    }


-- | Returns a typed link specifier as output.
lotlrsTypedLinkSpecifiers :: Lens' ListOutgoingTypedLinksResponse [TypedLinkSpecifier]
lotlrsTypedLinkSpecifiers = lens _lotlrsTypedLinkSpecifiers (\ s a -> s{_lotlrsTypedLinkSpecifiers = a}) . _Default . _Coerce

-- | The pagination token.
lotlrsNextToken :: Lens' ListOutgoingTypedLinksResponse (Maybe Text)
lotlrsNextToken = lens _lotlrsNextToken (\ s a -> s{_lotlrsNextToken = a})

-- | -- | The response status code.
lotlrsResponseStatus :: Lens' ListOutgoingTypedLinksResponse Int
lotlrsResponseStatus = lens _lotlrsResponseStatus (\ s a -> s{_lotlrsResponseStatus = a})

instance NFData ListOutgoingTypedLinksResponse where
