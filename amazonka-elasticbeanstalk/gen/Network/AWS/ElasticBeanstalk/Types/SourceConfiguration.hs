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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A specification for an environment configuration.
--
-- /See:/ 'newSourceConfiguration' smart constructor.
data SourceConfiguration = SourceConfiguration'
  { -- | The name of the configuration template.
    templateName :: Core.Maybe Core.Text,
    -- | The name of the application associated with the configuration.
    applicationName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { templateName = Core.Nothing,
      applicationName = Core.Nothing
    }

-- | The name of the configuration template.
sourceConfiguration_templateName :: Lens.Lens' SourceConfiguration (Core.Maybe Core.Text)
sourceConfiguration_templateName = Lens.lens (\SourceConfiguration' {templateName} -> templateName) (\s@SourceConfiguration' {} a -> s {templateName = a} :: SourceConfiguration)

-- | The name of the application associated with the configuration.
sourceConfiguration_applicationName :: Lens.Lens' SourceConfiguration (Core.Maybe Core.Text)
sourceConfiguration_applicationName = Lens.lens (\SourceConfiguration' {applicationName} -> applicationName) (\s@SourceConfiguration' {} a -> s {applicationName = a} :: SourceConfiguration)

instance Core.Hashable SourceConfiguration

instance Core.NFData SourceConfiguration

instance Core.ToQuery SourceConfiguration where
  toQuery SourceConfiguration' {..} =
    Core.mconcat
      [ "TemplateName" Core.=: templateName,
        "ApplicationName" Core.=: applicationName
      ]
