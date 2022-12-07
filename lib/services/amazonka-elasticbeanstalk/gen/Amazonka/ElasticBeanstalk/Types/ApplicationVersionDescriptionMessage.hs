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
-- Module      : Amazonka.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.ApplicationVersionDescription
import qualified Amazonka.Prelude as Prelude

-- | Result message wrapping a single description of an application version.
--
-- /See:/ 'newApplicationVersionDescriptionMessage' smart constructor.
data ApplicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage'
  { -- | The ApplicationVersionDescription of the application version.
    applicationVersion :: Prelude.Maybe ApplicationVersionDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The ApplicationVersionDescription of the application version.
applicationVersionDescriptionMessage_applicationVersion :: Lens.Lens' ApplicationVersionDescriptionMessage (Prelude.Maybe ApplicationVersionDescription)
applicationVersionDescriptionMessage_applicationVersion = Lens.lens (\ApplicationVersionDescriptionMessage' {applicationVersion} -> applicationVersion) (\s@ApplicationVersionDescriptionMessage' {} a -> s {applicationVersion = a} :: ApplicationVersionDescriptionMessage)

instance
  Data.FromXML
    ApplicationVersionDescriptionMessage
  where
  parseXML x =
    ApplicationVersionDescriptionMessage'
      Prelude.<$> (x Data..@? "ApplicationVersion")

instance
  Prelude.Hashable
    ApplicationVersionDescriptionMessage
  where
  hashWithSalt
    _salt
    ApplicationVersionDescriptionMessage' {..} =
      _salt `Prelude.hashWithSalt` applicationVersion

instance
  Prelude.NFData
    ApplicationVersionDescriptionMessage
  where
  rnf ApplicationVersionDescriptionMessage' {..} =
    Prelude.rnf applicationVersion
