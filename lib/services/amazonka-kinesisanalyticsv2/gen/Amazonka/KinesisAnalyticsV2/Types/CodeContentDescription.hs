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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.CodeContentDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.CodeContentDescription where

import qualified Amazonka.Core as Core
import Amazonka.KinesisAnalyticsV2.Types.S3ApplicationCodeLocationDescription
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes details about the code of a Kinesis Data Analytics
-- application.
--
-- /See:/ 'newCodeContentDescription' smart constructor.
data CodeContentDescription = CodeContentDescription'
  { -- | The S3 bucket Amazon Resource Name (ARN), file key, and object version
    -- of the application code stored in Amazon S3.
    s3ApplicationCodeLocationDescription :: Prelude.Maybe S3ApplicationCodeLocationDescription,
    -- | The checksum that can be used to validate zip-format code.
    codeMD5 :: Prelude.Maybe Prelude.Text,
    -- | The text-format code
    textContent :: Prelude.Maybe Prelude.Text,
    -- | The size in bytes of the application code. Can be used to validate
    -- zip-format code.
    codeSize :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeContentDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ApplicationCodeLocationDescription', 'codeContentDescription_s3ApplicationCodeLocationDescription' - The S3 bucket Amazon Resource Name (ARN), file key, and object version
-- of the application code stored in Amazon S3.
--
-- 'codeMD5', 'codeContentDescription_codeMD5' - The checksum that can be used to validate zip-format code.
--
-- 'textContent', 'codeContentDescription_textContent' - The text-format code
--
-- 'codeSize', 'codeContentDescription_codeSize' - The size in bytes of the application code. Can be used to validate
-- zip-format code.
newCodeContentDescription ::
  CodeContentDescription
newCodeContentDescription =
  CodeContentDescription'
    { s3ApplicationCodeLocationDescription =
        Prelude.Nothing,
      codeMD5 = Prelude.Nothing,
      textContent = Prelude.Nothing,
      codeSize = Prelude.Nothing
    }

-- | The S3 bucket Amazon Resource Name (ARN), file key, and object version
-- of the application code stored in Amazon S3.
codeContentDescription_s3ApplicationCodeLocationDescription :: Lens.Lens' CodeContentDescription (Prelude.Maybe S3ApplicationCodeLocationDescription)
codeContentDescription_s3ApplicationCodeLocationDescription = Lens.lens (\CodeContentDescription' {s3ApplicationCodeLocationDescription} -> s3ApplicationCodeLocationDescription) (\s@CodeContentDescription' {} a -> s {s3ApplicationCodeLocationDescription = a} :: CodeContentDescription)

-- | The checksum that can be used to validate zip-format code.
codeContentDescription_codeMD5 :: Lens.Lens' CodeContentDescription (Prelude.Maybe Prelude.Text)
codeContentDescription_codeMD5 = Lens.lens (\CodeContentDescription' {codeMD5} -> codeMD5) (\s@CodeContentDescription' {} a -> s {codeMD5 = a} :: CodeContentDescription)

-- | The text-format code
codeContentDescription_textContent :: Lens.Lens' CodeContentDescription (Prelude.Maybe Prelude.Text)
codeContentDescription_textContent = Lens.lens (\CodeContentDescription' {textContent} -> textContent) (\s@CodeContentDescription' {} a -> s {textContent = a} :: CodeContentDescription)

-- | The size in bytes of the application code. Can be used to validate
-- zip-format code.
codeContentDescription_codeSize :: Lens.Lens' CodeContentDescription (Prelude.Maybe Prelude.Natural)
codeContentDescription_codeSize = Lens.lens (\CodeContentDescription' {codeSize} -> codeSize) (\s@CodeContentDescription' {} a -> s {codeSize = a} :: CodeContentDescription)

instance Core.FromJSON CodeContentDescription where
  parseJSON =
    Core.withObject
      "CodeContentDescription"
      ( \x ->
          CodeContentDescription'
            Prelude.<$> (x Core..:? "S3ApplicationCodeLocationDescription")
            Prelude.<*> (x Core..:? "CodeMD5")
            Prelude.<*> (x Core..:? "TextContent")
            Prelude.<*> (x Core..:? "CodeSize")
      )

instance Prelude.Hashable CodeContentDescription

instance Prelude.NFData CodeContentDescription
