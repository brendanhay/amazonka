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
-- Module      : Network.AWS.CognitoIdentity.MergeDeveloperIdentities
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two users having different @IdentityId@s, existing in the same
-- identity pool, and identified by the same developer provider. You can
-- use this action to request that discrete users be merged and identified
-- as a single user in the Cognito environment. Cognito associates the
-- given source user (@SourceUserIdentifier@) with the @IdentityId@ of the
-- @DestinationUserIdentifier@. Only developer-authenticated users can be
-- merged. If the users to be merged are associated with the same public
-- provider, but as two different users, an exception will be thrown.
--
-- You must use AWS Developer credentials to call this API.
--
-- /See:/ <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_MergeDeveloperIdentities.html AWS API Reference> for MergeDeveloperIdentities.
module Network.AWS.CognitoIdentity.MergeDeveloperIdentities
    (
    -- * Creating a Request
      MergeDeveloperIdentities
    , mergeDeveloperIdentities
    -- * Request Lenses
    , mdiSourceUserIdentifier
    , mdiDestinationUserIdentifier
    , mdiDeveloperProviderName
    , mdiIdentityPoolId

    -- * Destructuring the Response
    , MergeDeveloperIdentitiesResponse
    , mergeDeveloperIdentitiesResponse
    -- * Response Lenses
    , mdirsIdentityId
    , mdirsStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.CognitoIdentity.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the @MergeDeveloperIdentities@ action.
--
-- /See:/ 'mergeDeveloperIdentities' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdiSourceUserIdentifier'
--
-- * 'mdiDestinationUserIdentifier'
--
-- * 'mdiDeveloperProviderName'
--
-- * 'mdiIdentityPoolId'
data MergeDeveloperIdentities = MergeDeveloperIdentities'
    { _mdiSourceUserIdentifier      :: !Text
    , _mdiDestinationUserIdentifier :: !Text
    , _mdiDeveloperProviderName     :: !Text
    , _mdiIdentityPoolId            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MergeDeveloperIdentities' smart constructor.
mergeDeveloperIdentities :: Text -> Text -> Text -> Text -> MergeDeveloperIdentities
mergeDeveloperIdentities pSourceUserIdentifier_ pDestinationUserIdentifier_ pDeveloperProviderName_ pIdentityPoolId_ =
    MergeDeveloperIdentities'
    { _mdiSourceUserIdentifier = pSourceUserIdentifier_
    , _mdiDestinationUserIdentifier = pDestinationUserIdentifier_
    , _mdiDeveloperProviderName = pDeveloperProviderName_
    , _mdiIdentityPoolId = pIdentityPoolId_
    }

-- | User identifier for the source user. The value should be a
-- @DeveloperUserIdentifier@.
mdiSourceUserIdentifier :: Lens' MergeDeveloperIdentities Text
mdiSourceUserIdentifier = lens _mdiSourceUserIdentifier (\ s a -> s{_mdiSourceUserIdentifier = a});

-- | User identifier for the destination user. The value should be a
-- @DeveloperUserIdentifier@.
mdiDestinationUserIdentifier :: Lens' MergeDeveloperIdentities Text
mdiDestinationUserIdentifier = lens _mdiDestinationUserIdentifier (\ s a -> s{_mdiDestinationUserIdentifier = a});

-- | The \"domain\" by which Cognito will refer to your users. This is a
-- (pseudo) domain name that you provide while creating an identity pool.
-- This name acts as a placeholder that allows your backend and the Cognito
-- service to communicate about the developer provider. For the
-- @DeveloperProviderName@, you can use letters as well as period (.),
-- underscore (_), and dash (-).
mdiDeveloperProviderName :: Lens' MergeDeveloperIdentities Text
mdiDeveloperProviderName = lens _mdiDeveloperProviderName (\ s a -> s{_mdiDeveloperProviderName = a});

-- | An identity pool ID in the format REGION:GUID.
mdiIdentityPoolId :: Lens' MergeDeveloperIdentities Text
mdiIdentityPoolId = lens _mdiIdentityPoolId (\ s a -> s{_mdiIdentityPoolId = a});

instance AWSRequest MergeDeveloperIdentities where
        type Sv MergeDeveloperIdentities = CognitoIdentity
        type Rs MergeDeveloperIdentities =
             MergeDeveloperIdentitiesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 MergeDeveloperIdentitiesResponse' <$>
                   (x .?> "IdentityId") <*> (pure (fromEnum s)))

instance ToHeaders MergeDeveloperIdentities where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.MergeDeveloperIdentities"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON MergeDeveloperIdentities where
        toJSON MergeDeveloperIdentities'{..}
          = object
              ["SourceUserIdentifier" .= _mdiSourceUserIdentifier,
               "DestinationUserIdentifier" .=
                 _mdiDestinationUserIdentifier,
               "DeveloperProviderName" .= _mdiDeveloperProviderName,
               "IdentityPoolId" .= _mdiIdentityPoolId]

instance ToPath MergeDeveloperIdentities where
        toPath = const "/"

instance ToQuery MergeDeveloperIdentities where
        toQuery = const mempty

-- | Returned in response to a successful @MergeDeveloperIdentities@ action.
--
-- /See:/ 'mergeDeveloperIdentitiesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdirsIdentityId'
--
-- * 'mdirsStatus'
data MergeDeveloperIdentitiesResponse = MergeDeveloperIdentitiesResponse'
    { _mdirsIdentityId :: !(Maybe Text)
    , _mdirsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MergeDeveloperIdentitiesResponse' smart constructor.
mergeDeveloperIdentitiesResponse :: Int -> MergeDeveloperIdentitiesResponse
mergeDeveloperIdentitiesResponse pStatus_ =
    MergeDeveloperIdentitiesResponse'
    { _mdirsIdentityId = Nothing
    , _mdirsStatus = pStatus_
    }

-- | A unique identifier in the format REGION:GUID.
mdirsIdentityId :: Lens' MergeDeveloperIdentitiesResponse (Maybe Text)
mdirsIdentityId = lens _mdirsIdentityId (\ s a -> s{_mdirsIdentityId = a});

-- | Undocumented member.
mdirsStatus :: Lens' MergeDeveloperIdentitiesResponse Int
mdirsStatus = lens _mdirsStatus (\ s a -> s{_mdirsStatus = a});
