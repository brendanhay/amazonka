{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppConfigData.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfigData.Lens
  ( -- * Operations

    -- ** GetLatestConfiguration
    getLatestConfiguration_configurationToken,
    getLatestConfigurationResponse_configuration,
    getLatestConfigurationResponse_contentType,
    getLatestConfigurationResponse_nextPollConfigurationToken,
    getLatestConfigurationResponse_nextPollIntervalInSeconds,
    getLatestConfigurationResponse_httpStatus,

    -- ** StartConfigurationSession
    startConfigurationSession_requiredMinimumPollIntervalInSeconds,
    startConfigurationSession_applicationIdentifier,
    startConfigurationSession_environmentIdentifier,
    startConfigurationSession_configurationProfileIdentifier,
    startConfigurationSessionResponse_initialConfigurationToken,
    startConfigurationSessionResponse_httpStatus,

    -- * Types
  )
where

import Amazonka.AppConfigData.GetLatestConfiguration
import Amazonka.AppConfigData.StartConfigurationSession
