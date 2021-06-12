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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationDescriptionMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationDescriptionMessage where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.ApplicationDescription
import qualified Network.AWS.Lens as Lens

-- | Result message containing a single description of an application.
--
-- /See:/ 'newApplicationDescriptionMessage' smart constructor.
data ApplicationDescriptionMessage = ApplicationDescriptionMessage'
  { -- | The ApplicationDescription of the application.
    application :: Core.Maybe ApplicationDescription
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplicationDescriptionMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'application', 'applicationDescriptionMessage_application' - The ApplicationDescription of the application.
newApplicationDescriptionMessage ::
  ApplicationDescriptionMessage
newApplicationDescriptionMessage =
  ApplicationDescriptionMessage'
    { application =
        Core.Nothing
    }

-- | The ApplicationDescription of the application.
applicationDescriptionMessage_application :: Lens.Lens' ApplicationDescriptionMessage (Core.Maybe ApplicationDescription)
applicationDescriptionMessage_application = Lens.lens (\ApplicationDescriptionMessage' {application} -> application) (\s@ApplicationDescriptionMessage' {} a -> s {application = a} :: ApplicationDescriptionMessage)

instance Core.FromXML ApplicationDescriptionMessage where
  parseXML x =
    ApplicationDescriptionMessage'
      Core.<$> (x Core..@? "Application")

instance Core.Hashable ApplicationDescriptionMessage

instance Core.NFData ApplicationDescriptionMessage
