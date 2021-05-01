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
-- Module      : Network.AWS.SWF.Types.DomainConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DomainConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the configuration settings of a domain.
--
-- /See:/ 'newDomainConfiguration' smart constructor.
data DomainConfiguration = DomainConfiguration'
  { -- | The retention period for workflow executions in this domain.
    workflowExecutionRetentionPeriodInDays :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DomainConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowExecutionRetentionPeriodInDays', 'domainConfiguration_workflowExecutionRetentionPeriodInDays' - The retention period for workflow executions in this domain.
newDomainConfiguration ::
  -- | 'workflowExecutionRetentionPeriodInDays'
  Prelude.Text ->
  DomainConfiguration
newDomainConfiguration
  pWorkflowExecutionRetentionPeriodInDays_ =
    DomainConfiguration'
      { workflowExecutionRetentionPeriodInDays =
          pWorkflowExecutionRetentionPeriodInDays_
      }

-- | The retention period for workflow executions in this domain.
domainConfiguration_workflowExecutionRetentionPeriodInDays :: Lens.Lens' DomainConfiguration Prelude.Text
domainConfiguration_workflowExecutionRetentionPeriodInDays = Lens.lens (\DomainConfiguration' {workflowExecutionRetentionPeriodInDays} -> workflowExecutionRetentionPeriodInDays) (\s@DomainConfiguration' {} a -> s {workflowExecutionRetentionPeriodInDays = a} :: DomainConfiguration)

instance Prelude.FromJSON DomainConfiguration where
  parseJSON =
    Prelude.withObject
      "DomainConfiguration"
      ( \x ->
          DomainConfiguration'
            Prelude.<$> ( x
                            Prelude..: "workflowExecutionRetentionPeriodInDays"
                        )
      )

instance Prelude.Hashable DomainConfiguration

instance Prelude.NFData DomainConfiguration
