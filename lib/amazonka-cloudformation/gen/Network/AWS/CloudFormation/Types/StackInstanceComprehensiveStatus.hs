{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus where

import Network.AWS.CloudFormation.Types.StackInstanceDetailedStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The detailed status of the stack instance.
--
--
--
-- /See:/ 'stackInstanceComprehensiveStatus' smart constructor.
newtype StackInstanceComprehensiveStatus = StackInstanceComprehensiveStatus'
  { _sicsDetailedStatus ::
      Maybe
        StackInstanceDetailedStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackInstanceComprehensiveStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sicsDetailedStatus' -     * @CANCELLED@ : The operation in the specified account and Region has been cancelled. This is either because a user has stopped the stack set operation, or because the failure tolerance of the stack set operation has been exceeded.     * @FAILED@ : The operation in the specified account and Region failed. If the stack set operation fails in enough accounts within a Region, the failure tolerance for the stack set operation as a whole might be exceeded.     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.     * @PENDING@ : The operation in the specified account and Region has yet to start.     * @RUNNING@ : The operation in the specified account and Region is currently in progress.     * @SUCCEEDED@ : The operation in the specified account and Region completed successfully.
stackInstanceComprehensiveStatus ::
  StackInstanceComprehensiveStatus
stackInstanceComprehensiveStatus =
  StackInstanceComprehensiveStatus' {_sicsDetailedStatus = Nothing}

-- |     * @CANCELLED@ : The operation in the specified account and Region has been cancelled. This is either because a user has stopped the stack set operation, or because the failure tolerance of the stack set operation has been exceeded.     * @FAILED@ : The operation in the specified account and Region failed. If the stack set operation fails in enough accounts within a Region, the failure tolerance for the stack set operation as a whole might be exceeded.     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.     * @PENDING@ : The operation in the specified account and Region has yet to start.     * @RUNNING@ : The operation in the specified account and Region is currently in progress.     * @SUCCEEDED@ : The operation in the specified account and Region completed successfully.
sicsDetailedStatus :: Lens' StackInstanceComprehensiveStatus (Maybe StackInstanceDetailedStatus)
sicsDetailedStatus = lens _sicsDetailedStatus (\s a -> s {_sicsDetailedStatus = a})

instance FromXML StackInstanceComprehensiveStatus where
  parseXML x =
    StackInstanceComprehensiveStatus' <$> (x .@? "DetailedStatus")

instance Hashable StackInstanceComprehensiveStatus

instance NFData StackInstanceComprehensiveStatus
