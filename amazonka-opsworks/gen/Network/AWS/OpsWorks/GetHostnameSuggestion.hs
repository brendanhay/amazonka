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
-- Module      : Network.AWS.OpsWorks.GetHostnameSuggestion
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a generated host name for the specified layer, based on the current
-- host name theme.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_GetHostnameSuggestion.html AWS API Reference> for GetHostnameSuggestion.
module Network.AWS.OpsWorks.GetHostnameSuggestion
    (
    -- * Creating a Request
      getHostnameSuggestion
    , GetHostnameSuggestion
    -- * Request Lenses
    , ghsLayerId

    -- * Destructuring the Response
    , getHostnameSuggestionResponse
    , GetHostnameSuggestionResponse
    -- * Response Lenses
    , ghsrsHostname
    , ghsrsLayerId
    , ghsrsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getHostnameSuggestion' smart constructor.
newtype GetHostnameSuggestion = GetHostnameSuggestion'
    { _ghsLayerId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetHostnameSuggestion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghsLayerId'
getHostnameSuggestion
    :: Text -- ^ 'ghsLayerId'
    -> GetHostnameSuggestion
getHostnameSuggestion pLayerId_ =
    GetHostnameSuggestion'
    { _ghsLayerId = pLayerId_
    }

-- | The layer ID.
ghsLayerId :: Lens' GetHostnameSuggestion Text
ghsLayerId = lens _ghsLayerId (\ s a -> s{_ghsLayerId = a});

instance AWSRequest GetHostnameSuggestion where
        type Sv GetHostnameSuggestion = OpsWorks
        type Rs GetHostnameSuggestion =
             GetHostnameSuggestionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetHostnameSuggestionResponse' <$>
                   (x .?> "Hostname") <*> (x .?> "LayerId") <*>
                     (pure (fromEnum s)))

instance ToHeaders GetHostnameSuggestion where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.GetHostnameSuggestion" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetHostnameSuggestion where
        toJSON GetHostnameSuggestion'{..}
          = object ["LayerId" .= _ghsLayerId]

instance ToPath GetHostnameSuggestion where
        toPath = const "/"

instance ToQuery GetHostnameSuggestion where
        toQuery = const mempty

-- | Contains the response to a 'GetHostnameSuggestion' request.
--
-- /See:/ 'getHostnameSuggestionResponse' smart constructor.
data GetHostnameSuggestionResponse = GetHostnameSuggestionResponse'
    { _ghsrsHostname :: !(Maybe Text)
    , _ghsrsLayerId  :: !(Maybe Text)
    , _ghsrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetHostnameSuggestionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghsrsHostname'
--
-- * 'ghsrsLayerId'
--
-- * 'ghsrsStatus'
getHostnameSuggestionResponse
    :: Int -- ^ 'ghsrsStatus'
    -> GetHostnameSuggestionResponse
getHostnameSuggestionResponse pStatus_ =
    GetHostnameSuggestionResponse'
    { _ghsrsHostname = Nothing
    , _ghsrsLayerId = Nothing
    , _ghsrsStatus = pStatus_
    }

-- | The generated host name.
ghsrsHostname :: Lens' GetHostnameSuggestionResponse (Maybe Text)
ghsrsHostname = lens _ghsrsHostname (\ s a -> s{_ghsrsHostname = a});

-- | The layer ID.
ghsrsLayerId :: Lens' GetHostnameSuggestionResponse (Maybe Text)
ghsrsLayerId = lens _ghsrsLayerId (\ s a -> s{_ghsrsLayerId = a});

-- | The response status code.
ghsrsStatus :: Lens' GetHostnameSuggestionResponse Int
ghsrsStatus = lens _ghsrsStatus (\ s a -> s{_ghsrsStatus = a});
