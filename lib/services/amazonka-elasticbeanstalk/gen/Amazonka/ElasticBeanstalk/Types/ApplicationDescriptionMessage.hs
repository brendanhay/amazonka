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
-- Module      : Amazonka.ElasticBeanstalk.Types.ApplicationDescriptionMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ApplicationDescriptionMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.ApplicationDescription
import qualified Amazonka.Prelude as Prelude

-- | Result message containing a single description of an application.
--
-- /See:/ 'newApplicationDescriptionMessage' smart constructor.
data ApplicationDescriptionMessage = ApplicationDescriptionMessage'
  { -- | The ApplicationDescription of the application.
    application :: Prelude.Maybe ApplicationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The ApplicationDescription of the application.
applicationDescriptionMessage_application :: Lens.Lens' ApplicationDescriptionMessage (Prelude.Maybe ApplicationDescription)
applicationDescriptionMessage_application = Lens.lens (\ApplicationDescriptionMessage' {application} -> application) (\s@ApplicationDescriptionMessage' {} a -> s {application = a} :: ApplicationDescriptionMessage)

instance Data.FromXML ApplicationDescriptionMessage where
  parseXML x =
    ApplicationDescriptionMessage'
      Prelude.<$> (x Data..@? "Application")

instance
  Prelude.Hashable
    ApplicationDescriptionMessage
  where
  hashWithSalt _salt ApplicationDescriptionMessage' {..} =
    _salt `Prelude.hashWithSalt` application

instance Prelude.NFData ApplicationDescriptionMessage where
  rnf ApplicationDescriptionMessage' {..} =
    Prelude.rnf application
