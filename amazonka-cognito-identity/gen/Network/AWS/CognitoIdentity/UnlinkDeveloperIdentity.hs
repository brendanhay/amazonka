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
-- Module      : Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a @DeveloperUserIdentifier@ from an existing identity. Unlinked developer users will be considered new identities next time they are seen. If, for a given Cognito identity, you remove all federated identities as well as the developer user identifier, the Cognito identity becomes inaccessible.
--
--
-- You must use AWS Developer credentials to call this API.
--
module Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
    (
    -- * Creating a Request
      unlinkDeveloperIdentity
    , UnlinkDeveloperIdentity
    -- * Request Lenses
    , udiIdentityId
    , udiIdentityPoolId
    , udiDeveloperProviderName
    , udiDeveloperUserIdentifier

    -- * Destructuring the Response
    , unlinkDeveloperIdentityResponse
    , UnlinkDeveloperIdentityResponse
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input to the @UnlinkDeveloperIdentity@ action.
--
--
--
-- /See:/ 'unlinkDeveloperIdentity' smart constructor.
data UnlinkDeveloperIdentity = UnlinkDeveloperIdentity'
  { _udiIdentityId              :: !Text
  , _udiIdentityPoolId          :: !Text
  , _udiDeveloperProviderName   :: !Text
  , _udiDeveloperUserIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnlinkDeveloperIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udiIdentityId' - A unique identifier in the format REGION:GUID.
--
-- * 'udiIdentityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- * 'udiDeveloperProviderName' - The "domain" by which Cognito will refer to your users.
--
-- * 'udiDeveloperUserIdentifier' - A unique ID used by your backend authentication process to identify a user.
unlinkDeveloperIdentity
    :: Text -- ^ 'udiIdentityId'
    -> Text -- ^ 'udiIdentityPoolId'
    -> Text -- ^ 'udiDeveloperProviderName'
    -> Text -- ^ 'udiDeveloperUserIdentifier'
    -> UnlinkDeveloperIdentity
unlinkDeveloperIdentity pIdentityId_ pIdentityPoolId_ pDeveloperProviderName_ pDeveloperUserIdentifier_ =
  UnlinkDeveloperIdentity'
    { _udiIdentityId = pIdentityId_
    , _udiIdentityPoolId = pIdentityPoolId_
    , _udiDeveloperProviderName = pDeveloperProviderName_
    , _udiDeveloperUserIdentifier = pDeveloperUserIdentifier_
    }


-- | A unique identifier in the format REGION:GUID.
udiIdentityId :: Lens' UnlinkDeveloperIdentity Text
udiIdentityId = lens _udiIdentityId (\ s a -> s{_udiIdentityId = a})

-- | An identity pool ID in the format REGION:GUID.
udiIdentityPoolId :: Lens' UnlinkDeveloperIdentity Text
udiIdentityPoolId = lens _udiIdentityPoolId (\ s a -> s{_udiIdentityPoolId = a})

-- | The "domain" by which Cognito will refer to your users.
udiDeveloperProviderName :: Lens' UnlinkDeveloperIdentity Text
udiDeveloperProviderName = lens _udiDeveloperProviderName (\ s a -> s{_udiDeveloperProviderName = a})

-- | A unique ID used by your backend authentication process to identify a user.
udiDeveloperUserIdentifier :: Lens' UnlinkDeveloperIdentity Text
udiDeveloperUserIdentifier = lens _udiDeveloperUserIdentifier (\ s a -> s{_udiDeveloperUserIdentifier = a})

instance AWSRequest UnlinkDeveloperIdentity where
        type Rs UnlinkDeveloperIdentity =
             UnlinkDeveloperIdentityResponse
        request = postJSON cognitoIdentity
        response
          = receiveNull UnlinkDeveloperIdentityResponse'

instance Hashable UnlinkDeveloperIdentity where

instance NFData UnlinkDeveloperIdentity where

instance ToHeaders UnlinkDeveloperIdentity where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.UnlinkDeveloperIdentity"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UnlinkDeveloperIdentity where
        toJSON UnlinkDeveloperIdentity'{..}
          = object
              (catMaybes
                 [Just ("IdentityId" .= _udiIdentityId),
                  Just ("IdentityPoolId" .= _udiIdentityPoolId),
                  Just
                    ("DeveloperProviderName" .=
                       _udiDeveloperProviderName),
                  Just
                    ("DeveloperUserIdentifier" .=
                       _udiDeveloperUserIdentifier)])

instance ToPath UnlinkDeveloperIdentity where
        toPath = const "/"

instance ToQuery UnlinkDeveloperIdentity where
        toQuery = const mempty

-- | /See:/ 'unlinkDeveloperIdentityResponse' smart constructor.
data UnlinkDeveloperIdentityResponse =
  UnlinkDeveloperIdentityResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnlinkDeveloperIdentityResponse' with the minimum fields required to make a request.
--
unlinkDeveloperIdentityResponse
    :: UnlinkDeveloperIdentityResponse
unlinkDeveloperIdentityResponse = UnlinkDeveloperIdentityResponse'


instance NFData UnlinkDeveloperIdentityResponse where
