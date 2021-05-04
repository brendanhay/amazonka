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
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDriftInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDriftInformation where

import Network.AWS.CloudFormation.Types.StackResourceDriftStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about whether the resource\'s actual configuration
-- differs, or has /drifted/, from its expected configuration.
--
-- /See:/ 'newStackResourceDriftInformation' smart constructor.
data StackResourceDriftInformation = StackResourceDriftInformation'
  { -- | When AWS CloudFormation last checked if the resource had drifted from
    -- its expected configuration.
    lastCheckTimestamp :: Prelude.Maybe Prelude.ISO8601,
    -- | Status of the resource\'s actual configuration compared to its expected
    -- configuration
    --
    -- -   @DELETED@: The resource differs from its expected configuration in
    --     that it has been deleted.
    --
    -- -   @MODIFIED@: The resource differs from its expected configuration.
    --
    -- -   @NOT_CHECKED@: AWS CloudFormation has not checked if the resource
    --     differs from its expected configuration.
    --
    --     Any resources that do not currently support drift detection have a
    --     status of @NOT_CHECKED@. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
    --
    -- -   @IN_SYNC@: The resources\'s actual configuration matches its
    --     expected configuration.
    stackResourceDriftStatus :: StackResourceDriftStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StackResourceDriftInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastCheckTimestamp', 'stackResourceDriftInformation_lastCheckTimestamp' - When AWS CloudFormation last checked if the resource had drifted from
-- its expected configuration.
--
-- 'stackResourceDriftStatus', 'stackResourceDriftInformation_stackResourceDriftStatus' - Status of the resource\'s actual configuration compared to its expected
-- configuration
--
-- -   @DELETED@: The resource differs from its expected configuration in
--     that it has been deleted.
--
-- -   @MODIFIED@: The resource differs from its expected configuration.
--
-- -   @NOT_CHECKED@: AWS CloudFormation has not checked if the resource
--     differs from its expected configuration.
--
--     Any resources that do not currently support drift detection have a
--     status of @NOT_CHECKED@. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
--
-- -   @IN_SYNC@: The resources\'s actual configuration matches its
--     expected configuration.
newStackResourceDriftInformation ::
  -- | 'stackResourceDriftStatus'
  StackResourceDriftStatus ->
  StackResourceDriftInformation
newStackResourceDriftInformation
  pStackResourceDriftStatus_ =
    StackResourceDriftInformation'
      { lastCheckTimestamp =
          Prelude.Nothing,
        stackResourceDriftStatus =
          pStackResourceDriftStatus_
      }

-- | When AWS CloudFormation last checked if the resource had drifted from
-- its expected configuration.
stackResourceDriftInformation_lastCheckTimestamp :: Lens.Lens' StackResourceDriftInformation (Prelude.Maybe Prelude.UTCTime)
stackResourceDriftInformation_lastCheckTimestamp = Lens.lens (\StackResourceDriftInformation' {lastCheckTimestamp} -> lastCheckTimestamp) (\s@StackResourceDriftInformation' {} a -> s {lastCheckTimestamp = a} :: StackResourceDriftInformation) Prelude.. Lens.mapping Prelude._Time

-- | Status of the resource\'s actual configuration compared to its expected
-- configuration
--
-- -   @DELETED@: The resource differs from its expected configuration in
--     that it has been deleted.
--
-- -   @MODIFIED@: The resource differs from its expected configuration.
--
-- -   @NOT_CHECKED@: AWS CloudFormation has not checked if the resource
--     differs from its expected configuration.
--
--     Any resources that do not currently support drift detection have a
--     status of @NOT_CHECKED@. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
--
-- -   @IN_SYNC@: The resources\'s actual configuration matches its
--     expected configuration.
stackResourceDriftInformation_stackResourceDriftStatus :: Lens.Lens' StackResourceDriftInformation StackResourceDriftStatus
stackResourceDriftInformation_stackResourceDriftStatus = Lens.lens (\StackResourceDriftInformation' {stackResourceDriftStatus} -> stackResourceDriftStatus) (\s@StackResourceDriftInformation' {} a -> s {stackResourceDriftStatus = a} :: StackResourceDriftInformation)

instance
  Prelude.FromXML
    StackResourceDriftInformation
  where
  parseXML x =
    StackResourceDriftInformation'
      Prelude.<$> (x Prelude..@? "LastCheckTimestamp")
      Prelude.<*> (x Prelude..@ "StackResourceDriftStatus")

instance
  Prelude.Hashable
    StackResourceDriftInformation

instance Prelude.NFData StackResourceDriftInformation
