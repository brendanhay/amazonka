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
-- Module      : Amazonka.ElasticBeanstalk.Types.SourceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.SourceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A specification for an environment configuration.
--
-- /See:/ 'newSourceConfiguration' smart constructor.
data SourceConfiguration = SourceConfiguration'
  { -- | The name of the configuration template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The name of the application associated with the configuration.
    applicationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'sourceConfiguration_templateName' - The name of the configuration template.
--
-- 'applicationName', 'sourceConfiguration_applicationName' - The name of the application associated with the configuration.
newSourceConfiguration ::
  SourceConfiguration
newSourceConfiguration =
  SourceConfiguration'
    { templateName =
        Prelude.Nothing,
      applicationName = Prelude.Nothing
    }

-- | The name of the configuration template.
sourceConfiguration_templateName :: Lens.Lens' SourceConfiguration (Prelude.Maybe Prelude.Text)
sourceConfiguration_templateName = Lens.lens (\SourceConfiguration' {templateName} -> templateName) (\s@SourceConfiguration' {} a -> s {templateName = a} :: SourceConfiguration)

-- | The name of the application associated with the configuration.
sourceConfiguration_applicationName :: Lens.Lens' SourceConfiguration (Prelude.Maybe Prelude.Text)
sourceConfiguration_applicationName = Lens.lens (\SourceConfiguration' {applicationName} -> applicationName) (\s@SourceConfiguration' {} a -> s {applicationName = a} :: SourceConfiguration)

instance Prelude.Hashable SourceConfiguration where
  hashWithSalt _salt SourceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData SourceConfiguration where
  rnf SourceConfiguration' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf applicationName

instance Data.ToQuery SourceConfiguration where
  toQuery SourceConfiguration' {..} =
    Prelude.mconcat
      [ "TemplateName" Data.=: templateName,
        "ApplicationName" Data.=: applicationName
      ]
