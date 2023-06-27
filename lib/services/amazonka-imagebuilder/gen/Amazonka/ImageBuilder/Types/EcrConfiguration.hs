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
-- Module      : Amazonka.ImageBuilder.Types.EcrConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.EcrConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings that Image Builder uses to configure the ECR repository and the
-- output container images that Amazon Inspector scans.
--
-- /See:/ 'newEcrConfiguration' smart constructor.
data EcrConfiguration = EcrConfiguration'
  { -- | Tags for Image Builder to apply to the output container image that &INS;
    -- scans. Tags can help you identify and manage your scanned images.
    containerTags :: Prelude.Maybe [Prelude.Text],
    -- | The name of the container repository that Amazon Inspector scans to
    -- identify findings for your container images. The name includes the path
    -- for the repository location. If you don’t provide this information,
    -- Image Builder creates a repository in your account named
    -- @image-builder-image-scanning-repository@ for vulnerability scans of
    -- your output container images.
    repositoryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcrConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerTags', 'ecrConfiguration_containerTags' - Tags for Image Builder to apply to the output container image that &INS;
-- scans. Tags can help you identify and manage your scanned images.
--
-- 'repositoryName', 'ecrConfiguration_repositoryName' - The name of the container repository that Amazon Inspector scans to
-- identify findings for your container images. The name includes the path
-- for the repository location. If you don’t provide this information,
-- Image Builder creates a repository in your account named
-- @image-builder-image-scanning-repository@ for vulnerability scans of
-- your output container images.
newEcrConfiguration ::
  EcrConfiguration
newEcrConfiguration =
  EcrConfiguration'
    { containerTags = Prelude.Nothing,
      repositoryName = Prelude.Nothing
    }

-- | Tags for Image Builder to apply to the output container image that &INS;
-- scans. Tags can help you identify and manage your scanned images.
ecrConfiguration_containerTags :: Lens.Lens' EcrConfiguration (Prelude.Maybe [Prelude.Text])
ecrConfiguration_containerTags = Lens.lens (\EcrConfiguration' {containerTags} -> containerTags) (\s@EcrConfiguration' {} a -> s {containerTags = a} :: EcrConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the container repository that Amazon Inspector scans to
-- identify findings for your container images. The name includes the path
-- for the repository location. If you don’t provide this information,
-- Image Builder creates a repository in your account named
-- @image-builder-image-scanning-repository@ for vulnerability scans of
-- your output container images.
ecrConfiguration_repositoryName :: Lens.Lens' EcrConfiguration (Prelude.Maybe Prelude.Text)
ecrConfiguration_repositoryName = Lens.lens (\EcrConfiguration' {repositoryName} -> repositoryName) (\s@EcrConfiguration' {} a -> s {repositoryName = a} :: EcrConfiguration)

instance Data.FromJSON EcrConfiguration where
  parseJSON =
    Data.withObject
      "EcrConfiguration"
      ( \x ->
          EcrConfiguration'
            Prelude.<$> (x Data..:? "containerTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "repositoryName")
      )

instance Prelude.Hashable EcrConfiguration where
  hashWithSalt _salt EcrConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` containerTags
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData EcrConfiguration where
  rnf EcrConfiguration' {..} =
    Prelude.rnf containerTags
      `Prelude.seq` Prelude.rnf repositoryName

instance Data.ToJSON EcrConfiguration where
  toJSON EcrConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("containerTags" Data..=) Prelude.<$> containerTags,
            ("repositoryName" Data..=)
              Prelude.<$> repositoryName
          ]
      )
