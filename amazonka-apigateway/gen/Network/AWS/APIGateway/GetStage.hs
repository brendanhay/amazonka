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
-- Module      : Network.AWS.APIGateway.GetStage
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a 'Stage' resource.
--
--
module Network.AWS.APIGateway.GetStage
    (
    -- * Creating a Request
      getStage
    , GetStage
    -- * Request Lenses
    , gssRestAPIId
    , gssStageName

    -- * Destructuring the Response
    , stage
    , Stage
    -- * Response Lenses
    , sDeploymentId
    , sVariables
    , sDocumentationVersion
    , sClientCertificateId
    , sCreatedDate
    , sCacheClusterStatus
    , sMethodSettings
    , sLastUpdatedDate
    , sCacheClusterSize
    , sCacheClusterEnabled
    , sStageName
    , sDescription
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Requests Amazon API Gateway to get information about a 'Stage' resource.
--
--
--
-- /See:/ 'getStage' smart constructor.
data GetStage = GetStage'
    { _gssRestAPIId :: !Text
    , _gssStageName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetStage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssRestAPIId' - The string identifier of the associated 'RestApi' .
--
-- * 'gssStageName' - The name of the 'Stage' resource to get information about.
getStage
    :: Text -- ^ 'gssRestAPIId'
    -> Text -- ^ 'gssStageName'
    -> GetStage
getStage pRestAPIId_ pStageName_ =
    GetStage'
    { _gssRestAPIId = pRestAPIId_
    , _gssStageName = pStageName_
    }

-- | The string identifier of the associated 'RestApi' .
gssRestAPIId :: Lens' GetStage Text
gssRestAPIId = lens _gssRestAPIId (\ s a -> s{_gssRestAPIId = a});

-- | The name of the 'Stage' resource to get information about.
gssStageName :: Lens' GetStage Text
gssStageName = lens _gssStageName (\ s a -> s{_gssStageName = a});

instance AWSRequest GetStage where
        type Rs GetStage = Stage
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetStage

instance NFData GetStage

instance ToHeaders GetStage where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetStage where
        toPath GetStage'{..}
          = mconcat
              ["/restapis/", toBS _gssRestAPIId, "/stages/",
               toBS _gssStageName]

instance ToQuery GetStage where
        toQuery = const mempty
