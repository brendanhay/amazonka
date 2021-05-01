{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus where

import Network.AWS.CloudFormation.Types.StackInstanceDetailedStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The detailed status of the stack instance.
--
-- /See:/ 'newStackInstanceComprehensiveStatus' smart constructor.
data StackInstanceComprehensiveStatus = StackInstanceComprehensiveStatus'
  { -- | -   @CANCELLED@: The operation in the specified account and Region has
    --     been cancelled. This is either because a user has stopped the stack
    --     set operation, or because the failure tolerance of the stack set
    --     operation has been exceeded.
    --
    -- -   @FAILED@: The operation in the specified account and Region failed.
    --     If the stack set operation fails in enough accounts within a Region,
    --     the failure tolerance for the stack set operation as a whole might
    --     be exceeded.
    --
    -- -   @INOPERABLE@: A @DeleteStackInstances@ operation has failed and left
    --     the stack in an unstable state. Stacks in this state are excluded
    --     from further @UpdateStackSet@ operations. You might need to perform
    --     a @DeleteStackInstances@ operation, with @RetainStacks@ set to
    --     @true@, to delete the stack instance, and then delete the stack
    --     manually.
    --
    -- -   @PENDING@: The operation in the specified account and Region has yet
    --     to start.
    --
    -- -   @RUNNING@: The operation in the specified account and Region is
    --     currently in progress.
    --
    -- -   @SUCCEEDED@: The operation in the specified account and Region
    --     completed successfully.
    detailedStatus :: Prelude.Maybe StackInstanceDetailedStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StackInstanceComprehensiveStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detailedStatus', 'stackInstanceComprehensiveStatus_detailedStatus' - -   @CANCELLED@: The operation in the specified account and Region has
--     been cancelled. This is either because a user has stopped the stack
--     set operation, or because the failure tolerance of the stack set
--     operation has been exceeded.
--
-- -   @FAILED@: The operation in the specified account and Region failed.
--     If the stack set operation fails in enough accounts within a Region,
--     the failure tolerance for the stack set operation as a whole might
--     be exceeded.
--
-- -   @INOPERABLE@: A @DeleteStackInstances@ operation has failed and left
--     the stack in an unstable state. Stacks in this state are excluded
--     from further @UpdateStackSet@ operations. You might need to perform
--     a @DeleteStackInstances@ operation, with @RetainStacks@ set to
--     @true@, to delete the stack instance, and then delete the stack
--     manually.
--
-- -   @PENDING@: The operation in the specified account and Region has yet
--     to start.
--
-- -   @RUNNING@: The operation in the specified account and Region is
--     currently in progress.
--
-- -   @SUCCEEDED@: The operation in the specified account and Region
--     completed successfully.
newStackInstanceComprehensiveStatus ::
  StackInstanceComprehensiveStatus
newStackInstanceComprehensiveStatus =
  StackInstanceComprehensiveStatus'
    { detailedStatus =
        Prelude.Nothing
    }

-- | -   @CANCELLED@: The operation in the specified account and Region has
--     been cancelled. This is either because a user has stopped the stack
--     set operation, or because the failure tolerance of the stack set
--     operation has been exceeded.
--
-- -   @FAILED@: The operation in the specified account and Region failed.
--     If the stack set operation fails in enough accounts within a Region,
--     the failure tolerance for the stack set operation as a whole might
--     be exceeded.
--
-- -   @INOPERABLE@: A @DeleteStackInstances@ operation has failed and left
--     the stack in an unstable state. Stacks in this state are excluded
--     from further @UpdateStackSet@ operations. You might need to perform
--     a @DeleteStackInstances@ operation, with @RetainStacks@ set to
--     @true@, to delete the stack instance, and then delete the stack
--     manually.
--
-- -   @PENDING@: The operation in the specified account and Region has yet
--     to start.
--
-- -   @RUNNING@: The operation in the specified account and Region is
--     currently in progress.
--
-- -   @SUCCEEDED@: The operation in the specified account and Region
--     completed successfully.
stackInstanceComprehensiveStatus_detailedStatus :: Lens.Lens' StackInstanceComprehensiveStatus (Prelude.Maybe StackInstanceDetailedStatus)
stackInstanceComprehensiveStatus_detailedStatus = Lens.lens (\StackInstanceComprehensiveStatus' {detailedStatus} -> detailedStatus) (\s@StackInstanceComprehensiveStatus' {} a -> s {detailedStatus = a} :: StackInstanceComprehensiveStatus)

instance
  Prelude.FromXML
    StackInstanceComprehensiveStatus
  where
  parseXML x =
    StackInstanceComprehensiveStatus'
      Prelude.<$> (x Prelude..@? "DetailedStatus")

instance
  Prelude.Hashable
    StackInstanceComprehensiveStatus

instance
  Prelude.NFData
    StackInstanceComprehensiveStatus
