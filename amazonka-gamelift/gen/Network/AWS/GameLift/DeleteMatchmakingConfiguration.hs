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
-- Module      : Network.AWS.GameLift.DeleteMatchmakingConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently removes a FlexMatch matchmaking configuration. To delete, specify the configuration name. A matchmaking configuration cannot be deleted if it is being used in any active matchmaking tickets.
--
--
-- Operations related to match configurations and rule sets include:
--
--     * 'CreateMatchmakingConfiguration'
--
--     * 'DescribeMatchmakingConfigurations'
--
--     * 'UpdateMatchmakingConfiguration'
--
--     * 'DeleteMatchmakingConfiguration'
--
--     * 'CreateMatchmakingRuleSet'
--
--     * 'DescribeMatchmakingRuleSets'
--
--     * 'ValidateMatchmakingRuleSet'
--
--
--
module Network.AWS.GameLift.DeleteMatchmakingConfiguration
    (
    -- * Creating a Request
      deleteMatchmakingConfiguration
    , DeleteMatchmakingConfiguration
    -- * Request Lenses
    , dmcName

    -- * Destructuring the Response
    , deleteMatchmakingConfigurationResponse
    , DeleteMatchmakingConfigurationResponse
    -- * Response Lenses
    , dmcrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'deleteMatchmakingConfiguration' smart constructor.
newtype DeleteMatchmakingConfiguration = DeleteMatchmakingConfiguration'
  { _dmcName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMatchmakingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmcName' - Unique identifier for a matchmaking configuration
deleteMatchmakingConfiguration
    :: Text -- ^ 'dmcName'
    -> DeleteMatchmakingConfiguration
deleteMatchmakingConfiguration pName_ =
  DeleteMatchmakingConfiguration' {_dmcName = pName_}


-- | Unique identifier for a matchmaking configuration
dmcName :: Lens' DeleteMatchmakingConfiguration Text
dmcName = lens _dmcName (\ s a -> s{_dmcName = a})

instance AWSRequest DeleteMatchmakingConfiguration
         where
        type Rs DeleteMatchmakingConfiguration =
             DeleteMatchmakingConfigurationResponse
        request = postJSON gameLift
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteMatchmakingConfigurationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteMatchmakingConfiguration
         where

instance NFData DeleteMatchmakingConfiguration where

instance ToHeaders DeleteMatchmakingConfiguration
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DeleteMatchmakingConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteMatchmakingConfiguration where
        toJSON DeleteMatchmakingConfiguration'{..}
          = object (catMaybes [Just ("Name" .= _dmcName)])

instance ToPath DeleteMatchmakingConfiguration where
        toPath = const "/"

instance ToQuery DeleteMatchmakingConfiguration where
        toQuery = const mempty

-- | /See:/ 'deleteMatchmakingConfigurationResponse' smart constructor.
newtype DeleteMatchmakingConfigurationResponse = DeleteMatchmakingConfigurationResponse'
  { _dmcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMatchmakingConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmcrsResponseStatus' - -- | The response status code.
deleteMatchmakingConfigurationResponse
    :: Int -- ^ 'dmcrsResponseStatus'
    -> DeleteMatchmakingConfigurationResponse
deleteMatchmakingConfigurationResponse pResponseStatus_ =
  DeleteMatchmakingConfigurationResponse'
    {_dmcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dmcrsResponseStatus :: Lens' DeleteMatchmakingConfigurationResponse Int
dmcrsResponseStatus = lens _dmcrsResponseStatus (\ s a -> s{_dmcrsResponseStatus = a})

instance NFData
           DeleteMatchmakingConfigurationResponse
         where
