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
-- Module      : Network.AWS.CloudDirectory.GetTypedLinkFacetInformation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the identity attribute order for a specific 'TypedLinkFacet' . For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
--
module Network.AWS.CloudDirectory.GetTypedLinkFacetInformation
    (
    -- * Creating a Request
      getTypedLinkFacetInformation
    , GetTypedLinkFacetInformation
    -- * Request Lenses
    , gtlfiSchemaARN
    , gtlfiName

    -- * Destructuring the Response
    , getTypedLinkFacetInformationResponse
    , GetTypedLinkFacetInformationResponse
    -- * Response Lenses
    , gtlfirsIdentityAttributeOrder
    , gtlfirsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTypedLinkFacetInformation' smart constructor.
data GetTypedLinkFacetInformation = GetTypedLinkFacetInformation'
  { _gtlfiSchemaARN :: !Text
  , _gtlfiName      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTypedLinkFacetInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtlfiSchemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- * 'gtlfiName' - The unique name of the typed link facet.
getTypedLinkFacetInformation
    :: Text -- ^ 'gtlfiSchemaARN'
    -> Text -- ^ 'gtlfiName'
    -> GetTypedLinkFacetInformation
getTypedLinkFacetInformation pSchemaARN_ pName_ =
  GetTypedLinkFacetInformation'
    {_gtlfiSchemaARN = pSchemaARN_, _gtlfiName = pName_}


-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
gtlfiSchemaARN :: Lens' GetTypedLinkFacetInformation Text
gtlfiSchemaARN = lens _gtlfiSchemaARN (\ s a -> s{_gtlfiSchemaARN = a})

-- | The unique name of the typed link facet.
gtlfiName :: Lens' GetTypedLinkFacetInformation Text
gtlfiName = lens _gtlfiName (\ s a -> s{_gtlfiName = a})

instance AWSRequest GetTypedLinkFacetInformation
         where
        type Rs GetTypedLinkFacetInformation =
             GetTypedLinkFacetInformationResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 GetTypedLinkFacetInformationResponse' <$>
                   (x .?> "IdentityAttributeOrder" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetTypedLinkFacetInformation where

instance NFData GetTypedLinkFacetInformation where

instance ToHeaders GetTypedLinkFacetInformation where
        toHeaders GetTypedLinkFacetInformation'{..}
          = mconcat ["x-amz-data-partition" =# _gtlfiSchemaARN]

instance ToJSON GetTypedLinkFacetInformation where
        toJSON GetTypedLinkFacetInformation'{..}
          = object (catMaybes [Just ("Name" .= _gtlfiName)])

instance ToPath GetTypedLinkFacetInformation where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/typedlink/facet/get"

instance ToQuery GetTypedLinkFacetInformation where
        toQuery = const mempty

-- | /See:/ 'getTypedLinkFacetInformationResponse' smart constructor.
data GetTypedLinkFacetInformationResponse = GetTypedLinkFacetInformationResponse'
  { _gtlfirsIdentityAttributeOrder :: !(Maybe [Text])
  , _gtlfirsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTypedLinkFacetInformationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtlfirsIdentityAttributeOrder' - The order of identity attributes for the facet, from most significant to least significant. The ability to filter typed links considers the order that the attributes are defined on the typed link facet. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range. Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls. For more information about identity attributes, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
-- * 'gtlfirsResponseStatus' - -- | The response status code.
getTypedLinkFacetInformationResponse
    :: Int -- ^ 'gtlfirsResponseStatus'
    -> GetTypedLinkFacetInformationResponse
getTypedLinkFacetInformationResponse pResponseStatus_ =
  GetTypedLinkFacetInformationResponse'
    { _gtlfirsIdentityAttributeOrder = Nothing
    , _gtlfirsResponseStatus = pResponseStatus_
    }


-- | The order of identity attributes for the facet, from most significant to least significant. The ability to filter typed links considers the order that the attributes are defined on the typed link facet. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range. Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls. For more information about identity attributes, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
gtlfirsIdentityAttributeOrder :: Lens' GetTypedLinkFacetInformationResponse [Text]
gtlfirsIdentityAttributeOrder = lens _gtlfirsIdentityAttributeOrder (\ s a -> s{_gtlfirsIdentityAttributeOrder = a}) . _Default . _Coerce

-- | -- | The response status code.
gtlfirsResponseStatus :: Lens' GetTypedLinkFacetInformationResponse Int
gtlfirsResponseStatus = lens _gtlfirsResponseStatus (\ s a -> s{_gtlfirsResponseStatus = a})

instance NFData GetTypedLinkFacetInformationResponse
         where
