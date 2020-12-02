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
-- Module      : Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an origin access identity.
--
--
module Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
    (
    -- * Creating a Request
      deleteCloudFrontOriginAccessIdentity
    , DeleteCloudFrontOriginAccessIdentity
    -- * Request Lenses
    , dcfoaiIfMatch
    , dcfoaiId

    -- * Destructuring the Response
    , deleteCloudFrontOriginAccessIdentityResponse
    , DeleteCloudFrontOriginAccessIdentityResponse
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Deletes a origin access identity.
--
--
--
-- /See:/ 'deleteCloudFrontOriginAccessIdentity' smart constructor.
data DeleteCloudFrontOriginAccessIdentity = DeleteCloudFrontOriginAccessIdentity'
  { _dcfoaiIfMatch :: !(Maybe Text)
  , _dcfoaiId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCloudFrontOriginAccessIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcfoaiIfMatch' - The value of the @ETag@ header you received from a previous @GET@ or @PUT@ request. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'dcfoaiId' - The origin access identity's ID.
deleteCloudFrontOriginAccessIdentity
    :: Text -- ^ 'dcfoaiId'
    -> DeleteCloudFrontOriginAccessIdentity
deleteCloudFrontOriginAccessIdentity pId_ =
  DeleteCloudFrontOriginAccessIdentity'
    {_dcfoaiIfMatch = Nothing, _dcfoaiId = pId_}


-- | The value of the @ETag@ header you received from a previous @GET@ or @PUT@ request. For example: @E2QWRUHAPOMQZL@ .
dcfoaiIfMatch :: Lens' DeleteCloudFrontOriginAccessIdentity (Maybe Text)
dcfoaiIfMatch = lens _dcfoaiIfMatch (\ s a -> s{_dcfoaiIfMatch = a})

-- | The origin access identity's ID.
dcfoaiId :: Lens' DeleteCloudFrontOriginAccessIdentity Text
dcfoaiId = lens _dcfoaiId (\ s a -> s{_dcfoaiId = a})

instance AWSRequest
           DeleteCloudFrontOriginAccessIdentity
         where
        type Rs DeleteCloudFrontOriginAccessIdentity =
             DeleteCloudFrontOriginAccessIdentityResponse
        request = delete cloudFront
        response
          = receiveNull
              DeleteCloudFrontOriginAccessIdentityResponse'

instance Hashable
           DeleteCloudFrontOriginAccessIdentity
         where

instance NFData DeleteCloudFrontOriginAccessIdentity
         where

instance ToHeaders
           DeleteCloudFrontOriginAccessIdentity
         where
        toHeaders DeleteCloudFrontOriginAccessIdentity'{..}
          = mconcat ["If-Match" =# _dcfoaiIfMatch]

instance ToPath DeleteCloudFrontOriginAccessIdentity
         where
        toPath DeleteCloudFrontOriginAccessIdentity'{..}
          = mconcat
              ["/2017-10-30/origin-access-identity/cloudfront/",
               toBS _dcfoaiId]

instance ToQuery DeleteCloudFrontOriginAccessIdentity
         where
        toQuery = const mempty

-- | /See:/ 'deleteCloudFrontOriginAccessIdentityResponse' smart constructor.
data DeleteCloudFrontOriginAccessIdentityResponse =
  DeleteCloudFrontOriginAccessIdentityResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCloudFrontOriginAccessIdentityResponse' with the minimum fields required to make a request.
--
deleteCloudFrontOriginAccessIdentityResponse
    :: DeleteCloudFrontOriginAccessIdentityResponse
deleteCloudFrontOriginAccessIdentityResponse =
  DeleteCloudFrontOriginAccessIdentityResponse'


instance NFData
           DeleteCloudFrontOriginAccessIdentityResponse
         where
