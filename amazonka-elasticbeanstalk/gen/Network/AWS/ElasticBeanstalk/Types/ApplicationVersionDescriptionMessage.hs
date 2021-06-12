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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription
import qualified Network.AWS.Lens as Lens

-- | Result message wrapping a single description of an application version.
--
-- /See:/ 'newApplicationVersionDescriptionMessage' smart constructor.
data ApplicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage'
  { -- | The ApplicationVersionDescription of the application version.
    applicationVersion :: Core.Maybe ApplicationVersionDescription
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplicationVersionDescriptionMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationVersion', 'applicationVersionDescriptionMessage_applicationVersion' - The ApplicationVersionDescription of the application version.
newApplicationVersionDescriptionMessage ::
  ApplicationVersionDescriptionMessage
newApplicationVersionDescriptionMessage =
  ApplicationVersionDescriptionMessage'
    { applicationVersion =
        Core.Nothing
    }

-- | The ApplicationVersionDescription of the application version.
applicationVersionDescriptionMessage_applicationVersion :: Lens.Lens' ApplicationVersionDescriptionMessage (Core.Maybe ApplicationVersionDescription)
applicationVersionDescriptionMessage_applicationVersion = Lens.lens (\ApplicationVersionDescriptionMessage' {applicationVersion} -> applicationVersion) (\s@ApplicationVersionDescriptionMessage' {} a -> s {applicationVersion = a} :: ApplicationVersionDescriptionMessage)

instance
  Core.FromXML
    ApplicationVersionDescriptionMessage
  where
  parseXML x =
    ApplicationVersionDescriptionMessage'
      Core.<$> (x Core..@? "ApplicationVersion")

instance
  Core.Hashable
    ApplicationVersionDescriptionMessage

instance
  Core.NFData
    ApplicationVersionDescriptionMessage
