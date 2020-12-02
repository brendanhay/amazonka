{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DomainConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the configuration settings of a domain.
--
--
--
-- /See:/ 'domainConfiguration' smart constructor.
newtype DomainConfiguration = DomainConfiguration'
  { _dcWorkflowExecutionRetentionPeriodInDays ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcWorkflowExecutionRetentionPeriodInDays' - The retention period for workflow executions in this domain.
domainConfiguration ::
  -- | 'dcWorkflowExecutionRetentionPeriodInDays'
  Text ->
  DomainConfiguration
domainConfiguration pWorkflowExecutionRetentionPeriodInDays_ =
  DomainConfiguration'
    { _dcWorkflowExecutionRetentionPeriodInDays =
        pWorkflowExecutionRetentionPeriodInDays_
    }

-- | The retention period for workflow executions in this domain.
dcWorkflowExecutionRetentionPeriodInDays :: Lens' DomainConfiguration Text
dcWorkflowExecutionRetentionPeriodInDays = lens _dcWorkflowExecutionRetentionPeriodInDays (\s a -> s {_dcWorkflowExecutionRetentionPeriodInDays = a})

instance FromJSON DomainConfiguration where
  parseJSON =
    withObject
      "DomainConfiguration"
      ( \x ->
          DomainConfiguration'
            <$> (x .: "workflowExecutionRetentionPeriodInDays")
      )

instance Hashable DomainConfiguration

instance NFData DomainConfiguration
