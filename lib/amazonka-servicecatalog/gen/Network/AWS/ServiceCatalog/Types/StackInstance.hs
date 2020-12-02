{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.StackInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.StackInstance where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.StackInstanceStatus

-- | An AWS CloudFormation stack, in a specific account and region, that's part of a stack set operation. A stack instance is a reference to an attempted or actual stack in a given account within a given region. A stack instance can exist without a stack—for example, if the stack couldn't be created for some reason. A stack instance is associated with only one stack set. Each stack instance contains the ID of its associated stack set, as well as the ID of the actual stack and the stack status.
--
--
--
-- /See:/ 'stackInstance' smart constructor.
data StackInstance = StackInstance'
  { _siAccount :: !(Maybe Text),
    _siRegion :: !(Maybe Text),
    _siStackInstanceStatus :: !(Maybe StackInstanceStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siAccount' - The name of the AWS account that the stack instance is associated with.
--
-- * 'siRegion' - The name of the AWS region that the stack instance is associated with.
--
-- * 'siStackInstanceStatus' - The status of the stack instance, in terms of its synchronization with its associated stack set.      * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to true, to delete the stack instance, and then delete the stack manually.      * @OUTDATED@ : The stack isn't currently up to date with the stack set because either the associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation, or the stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.     * @CURRENT@ : The stack is currently up to date with the stack set.
stackInstance ::
  StackInstance
stackInstance =
  StackInstance'
    { _siAccount = Nothing,
      _siRegion = Nothing,
      _siStackInstanceStatus = Nothing
    }

-- | The name of the AWS account that the stack instance is associated with.
siAccount :: Lens' StackInstance (Maybe Text)
siAccount = lens _siAccount (\s a -> s {_siAccount = a})

-- | The name of the AWS region that the stack instance is associated with.
siRegion :: Lens' StackInstance (Maybe Text)
siRegion = lens _siRegion (\s a -> s {_siRegion = a})

-- | The status of the stack instance, in terms of its synchronization with its associated stack set.      * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to true, to delete the stack instance, and then delete the stack manually.      * @OUTDATED@ : The stack isn't currently up to date with the stack set because either the associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation, or the stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.     * @CURRENT@ : The stack is currently up to date with the stack set.
siStackInstanceStatus :: Lens' StackInstance (Maybe StackInstanceStatus)
siStackInstanceStatus = lens _siStackInstanceStatus (\s a -> s {_siStackInstanceStatus = a})

instance FromJSON StackInstance where
  parseJSON =
    withObject
      "StackInstance"
      ( \x ->
          StackInstance'
            <$> (x .:? "Account")
            <*> (x .:? "Region")
            <*> (x .:? "StackInstanceStatus")
      )

instance Hashable StackInstance

instance NFData StackInstance
