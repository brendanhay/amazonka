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
-- Module      : Network.AWS.SageMaker.Types.ImageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.RepositoryAccessMode

-- | Specifies whether the model container is in Amazon ECR or a private
-- Docker registry accessible from your Amazon Virtual Private Cloud (VPC).
--
-- /See:/ 'newImageConfig' smart constructor.
data ImageConfig = ImageConfig'
  { -- | Set this to one of the following values:
    --
    -- -   @Platform@ - The model image is hosted in Amazon ECR.
    --
    -- -   @Vpc@ - The model image is hosted in a private Docker registry in
    --     your VPC.
    repositoryAccessMode :: RepositoryAccessMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryAccessMode', 'imageConfig_repositoryAccessMode' - Set this to one of the following values:
--
-- -   @Platform@ - The model image is hosted in Amazon ECR.
--
-- -   @Vpc@ - The model image is hosted in a private Docker registry in
--     your VPC.
newImageConfig ::
  -- | 'repositoryAccessMode'
  RepositoryAccessMode ->
  ImageConfig
newImageConfig pRepositoryAccessMode_ =
  ImageConfig'
    { repositoryAccessMode =
        pRepositoryAccessMode_
    }

-- | Set this to one of the following values:
--
-- -   @Platform@ - The model image is hosted in Amazon ECR.
--
-- -   @Vpc@ - The model image is hosted in a private Docker registry in
--     your VPC.
imageConfig_repositoryAccessMode :: Lens.Lens' ImageConfig RepositoryAccessMode
imageConfig_repositoryAccessMode = Lens.lens (\ImageConfig' {repositoryAccessMode} -> repositoryAccessMode) (\s@ImageConfig' {} a -> s {repositoryAccessMode = a} :: ImageConfig)

instance Prelude.FromJSON ImageConfig where
  parseJSON =
    Prelude.withObject
      "ImageConfig"
      ( \x ->
          ImageConfig'
            Prelude.<$> (x Prelude..: "RepositoryAccessMode")
      )

instance Prelude.Hashable ImageConfig

instance Prelude.NFData ImageConfig

instance Prelude.ToJSON ImageConfig where
  toJSON ImageConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RepositoryAccessMode"
                  Prelude..= repositoryAccessMode
              )
          ]
      )
