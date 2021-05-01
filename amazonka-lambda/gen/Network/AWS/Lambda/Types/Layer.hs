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
-- Module      : Network.AWS.Lambda.Types.Layer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Layer where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer>.
--
-- /See:/ 'newLayer' smart constructor.
data Layer = Layer'
  { -- | The Amazon Resource Name (ARN) for a signing profile version.
    signingProfileVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the function layer.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a signing job.
    signingJobArn :: Prelude.Maybe Prelude.Text,
    -- | The size of the layer archive in bytes.
    codeSize :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Layer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingProfileVersionArn', 'layer_signingProfileVersionArn' - The Amazon Resource Name (ARN) for a signing profile version.
--
-- 'arn', 'layer_arn' - The Amazon Resource Name (ARN) of the function layer.
--
-- 'signingJobArn', 'layer_signingJobArn' - The Amazon Resource Name (ARN) of a signing job.
--
-- 'codeSize', 'layer_codeSize' - The size of the layer archive in bytes.
newLayer ::
  Layer
newLayer =
  Layer'
    { signingProfileVersionArn = Prelude.Nothing,
      arn = Prelude.Nothing,
      signingJobArn = Prelude.Nothing,
      codeSize = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for a signing profile version.
layer_signingProfileVersionArn :: Lens.Lens' Layer (Prelude.Maybe Prelude.Text)
layer_signingProfileVersionArn = Lens.lens (\Layer' {signingProfileVersionArn} -> signingProfileVersionArn) (\s@Layer' {} a -> s {signingProfileVersionArn = a} :: Layer)

-- | The Amazon Resource Name (ARN) of the function layer.
layer_arn :: Lens.Lens' Layer (Prelude.Maybe Prelude.Text)
layer_arn = Lens.lens (\Layer' {arn} -> arn) (\s@Layer' {} a -> s {arn = a} :: Layer)

-- | The Amazon Resource Name (ARN) of a signing job.
layer_signingJobArn :: Lens.Lens' Layer (Prelude.Maybe Prelude.Text)
layer_signingJobArn = Lens.lens (\Layer' {signingJobArn} -> signingJobArn) (\s@Layer' {} a -> s {signingJobArn = a} :: Layer)

-- | The size of the layer archive in bytes.
layer_codeSize :: Lens.Lens' Layer (Prelude.Maybe Prelude.Integer)
layer_codeSize = Lens.lens (\Layer' {codeSize} -> codeSize) (\s@Layer' {} a -> s {codeSize = a} :: Layer)

instance Prelude.FromJSON Layer where
  parseJSON =
    Prelude.withObject
      "Layer"
      ( \x ->
          Layer'
            Prelude.<$> (x Prelude..:? "SigningProfileVersionArn")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "SigningJobArn")
            Prelude.<*> (x Prelude..:? "CodeSize")
      )

instance Prelude.Hashable Layer

instance Prelude.NFData Layer
