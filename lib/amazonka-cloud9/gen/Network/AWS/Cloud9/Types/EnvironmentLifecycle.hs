{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.EnvironmentLifecycle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.EnvironmentLifecycle where

import Network.AWS.Cloud9.Types.EnvironmentLifecycleStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the current creation or deletion lifecycle state of an AWS Cloud9 development environment.
--
--
--
-- /See:/ 'environmentLifecycle' smart constructor.
data EnvironmentLifecycle = EnvironmentLifecycle'
  { _elStatus ::
      !(Maybe EnvironmentLifecycleStatus),
    _elFailureResource :: !(Maybe Text),
    _elReason :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnvironmentLifecycle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elStatus' - The current creation or deletion lifecycle state of the environment.     * @CREATING@ : The environment is in the process of being created.     * @CREATED@ : The environment was successfully created.     * @CREATE_FAILED@ : The environment failed to be created.     * @DELETING@ : The environment is in the process of being deleted.     * @DELETE_FAILED@ : The environment failed to delete.
--
-- * 'elFailureResource' - If the environment failed to delete, the Amazon Resource Name (ARN) of the related AWS resource.
--
-- * 'elReason' - Any informational message about the lifecycle state of the environment.
environmentLifecycle ::
  EnvironmentLifecycle
environmentLifecycle =
  EnvironmentLifecycle'
    { _elStatus = Nothing,
      _elFailureResource = Nothing,
      _elReason = Nothing
    }

-- | The current creation or deletion lifecycle state of the environment.     * @CREATING@ : The environment is in the process of being created.     * @CREATED@ : The environment was successfully created.     * @CREATE_FAILED@ : The environment failed to be created.     * @DELETING@ : The environment is in the process of being deleted.     * @DELETE_FAILED@ : The environment failed to delete.
elStatus :: Lens' EnvironmentLifecycle (Maybe EnvironmentLifecycleStatus)
elStatus = lens _elStatus (\s a -> s {_elStatus = a})

-- | If the environment failed to delete, the Amazon Resource Name (ARN) of the related AWS resource.
elFailureResource :: Lens' EnvironmentLifecycle (Maybe Text)
elFailureResource = lens _elFailureResource (\s a -> s {_elFailureResource = a})

-- | Any informational message about the lifecycle state of the environment.
elReason :: Lens' EnvironmentLifecycle (Maybe Text)
elReason = lens _elReason (\s a -> s {_elReason = a})

instance FromJSON EnvironmentLifecycle where
  parseJSON =
    withObject
      "EnvironmentLifecycle"
      ( \x ->
          EnvironmentLifecycle'
            <$> (x .:? "status")
            <*> (x .:? "failureResource")
            <*> (x .:? "reason")
      )

instance Hashable EnvironmentLifecycle

instance NFData EnvironmentLifecycle
