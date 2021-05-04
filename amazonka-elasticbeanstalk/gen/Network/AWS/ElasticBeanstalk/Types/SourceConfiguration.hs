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
-- Module      : Network.AWS.ElasticBeanstalk.Types.SourceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SourceConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A specification for an environment configuration.
--
-- /See:/ 'newSourceConfiguration' smart constructor.
data SourceConfiguration = SourceConfiguration'
  { -- | The name of the configuration template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The name of the application associated with the configuration.
    applicationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable SourceConfiguration

instance Prelude.NFData SourceConfiguration

instance Prelude.ToQuery SourceConfiguration where
  toQuery SourceConfiguration' {..} =
    Prelude.mconcat
      [ "TemplateName" Prelude.=: templateName,
        "ApplicationName" Prelude.=: applicationName
      ]
