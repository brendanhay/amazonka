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
-- Module      : Amazonka.Wisdom.Types.RenderingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.RenderingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about how to render the content.
--
-- /See:/ 'newRenderingConfiguration' smart constructor.
data RenderingConfiguration = RenderingConfiguration'
  { -- | A URI template containing exactly one variable in
    -- @${variableName} @format. This can only be set for @EXTERNAL@ knowledge
    -- bases. For Salesforce and ServiceNow, the variable must be one of the
    -- following:
    --
    -- -   Salesforce: @Id@, @ArticleNumber@, @VersionNumber@, @Title@,
    --     @PublishStatus@, or @IsDeleted@
    --
    -- -   ServiceNow: @number@, @short_description@, @sys_mod_count@,
    --     @workflow_state@, or @active@
    --
    -- >  <p>The variable is replaced with the actual value for a piece of content when calling <a href="https://docs.aws.amazon.com/wisdom/latest/APIReference/API_GetContent.html">GetContent</a>. </p>
    templateUri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RenderingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateUri', 'renderingConfiguration_templateUri' - A URI template containing exactly one variable in
-- @${variableName} @format. This can only be set for @EXTERNAL@ knowledge
-- bases. For Salesforce and ServiceNow, the variable must be one of the
-- following:
--
-- -   Salesforce: @Id@, @ArticleNumber@, @VersionNumber@, @Title@,
--     @PublishStatus@, or @IsDeleted@
--
-- -   ServiceNow: @number@, @short_description@, @sys_mod_count@,
--     @workflow_state@, or @active@
--
-- >  <p>The variable is replaced with the actual value for a piece of content when calling <a href="https://docs.aws.amazon.com/wisdom/latest/APIReference/API_GetContent.html">GetContent</a>. </p>
newRenderingConfiguration ::
  RenderingConfiguration
newRenderingConfiguration =
  RenderingConfiguration'
    { templateUri =
        Prelude.Nothing
    }

-- | A URI template containing exactly one variable in
-- @${variableName} @format. This can only be set for @EXTERNAL@ knowledge
-- bases. For Salesforce and ServiceNow, the variable must be one of the
-- following:
--
-- -   Salesforce: @Id@, @ArticleNumber@, @VersionNumber@, @Title@,
--     @PublishStatus@, or @IsDeleted@
--
-- -   ServiceNow: @number@, @short_description@, @sys_mod_count@,
--     @workflow_state@, or @active@
--
-- >  <p>The variable is replaced with the actual value for a piece of content when calling <a href="https://docs.aws.amazon.com/wisdom/latest/APIReference/API_GetContent.html">GetContent</a>. </p>
renderingConfiguration_templateUri :: Lens.Lens' RenderingConfiguration (Prelude.Maybe Prelude.Text)
renderingConfiguration_templateUri = Lens.lens (\RenderingConfiguration' {templateUri} -> templateUri) (\s@RenderingConfiguration' {} a -> s {templateUri = a} :: RenderingConfiguration)

instance Data.FromJSON RenderingConfiguration where
  parseJSON =
    Data.withObject
      "RenderingConfiguration"
      ( \x ->
          RenderingConfiguration'
            Prelude.<$> (x Data..:? "templateUri")
      )

instance Prelude.Hashable RenderingConfiguration where
  hashWithSalt _salt RenderingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` templateUri

instance Prelude.NFData RenderingConfiguration where
  rnf RenderingConfiguration' {..} =
    Prelude.rnf templateUri

instance Data.ToJSON RenderingConfiguration where
  toJSON RenderingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("templateUri" Data..=) Prelude.<$> templateUri]
      )
