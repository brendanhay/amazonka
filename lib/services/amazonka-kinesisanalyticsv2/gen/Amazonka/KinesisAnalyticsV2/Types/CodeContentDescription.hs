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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.CodeContentDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.S3ApplicationCodeLocationDescription
import qualified Amazonka.Prelude as Prelude

-- | Describes details about the code of a Kinesis Data Analytics
-- application.
--
-- /See:/ 'newCodeContentDescription' smart constructor.
data CodeContentDescription = CodeContentDescription'
  { -- | The text-format code
    textContent :: Prelude.Maybe Prelude.Text,
    -- | The checksum that can be used to validate zip-format code.
    codeMD5 :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket Amazon Resource Name (ARN), file key, and object version
    -- of the application code stored in Amazon S3.
    s3ApplicationCodeLocationDescription :: Prelude.Maybe S3ApplicationCodeLocationDescription,
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
-- 'textContent', 'codeContentDescription_textContent' - The text-format code
--
-- 'codeMD5', 'codeContentDescription_codeMD5' - The checksum that can be used to validate zip-format code.
--
-- 's3ApplicationCodeLocationDescription', 'codeContentDescription_s3ApplicationCodeLocationDescription' - The S3 bucket Amazon Resource Name (ARN), file key, and object version
-- of the application code stored in Amazon S3.
--
-- 'codeSize', 'codeContentDescription_codeSize' - The size in bytes of the application code. Can be used to validate
-- zip-format code.
newCodeContentDescription ::
  CodeContentDescription
newCodeContentDescription =
  CodeContentDescription'
    { textContent =
        Prelude.Nothing,
      codeMD5 = Prelude.Nothing,
      s3ApplicationCodeLocationDescription =
        Prelude.Nothing,
      codeSize = Prelude.Nothing
    }

-- | The text-format code
codeContentDescription_textContent :: Lens.Lens' CodeContentDescription (Prelude.Maybe Prelude.Text)
codeContentDescription_textContent = Lens.lens (\CodeContentDescription' {textContent} -> textContent) (\s@CodeContentDescription' {} a -> s {textContent = a} :: CodeContentDescription)

-- | The checksum that can be used to validate zip-format code.
codeContentDescription_codeMD5 :: Lens.Lens' CodeContentDescription (Prelude.Maybe Prelude.Text)
codeContentDescription_codeMD5 = Lens.lens (\CodeContentDescription' {codeMD5} -> codeMD5) (\s@CodeContentDescription' {} a -> s {codeMD5 = a} :: CodeContentDescription)

-- | The S3 bucket Amazon Resource Name (ARN), file key, and object version
-- of the application code stored in Amazon S3.
codeContentDescription_s3ApplicationCodeLocationDescription :: Lens.Lens' CodeContentDescription (Prelude.Maybe S3ApplicationCodeLocationDescription)
codeContentDescription_s3ApplicationCodeLocationDescription = Lens.lens (\CodeContentDescription' {s3ApplicationCodeLocationDescription} -> s3ApplicationCodeLocationDescription) (\s@CodeContentDescription' {} a -> s {s3ApplicationCodeLocationDescription = a} :: CodeContentDescription)

-- | The size in bytes of the application code. Can be used to validate
-- zip-format code.
codeContentDescription_codeSize :: Lens.Lens' CodeContentDescription (Prelude.Maybe Prelude.Natural)
codeContentDescription_codeSize = Lens.lens (\CodeContentDescription' {codeSize} -> codeSize) (\s@CodeContentDescription' {} a -> s {codeSize = a} :: CodeContentDescription)

instance Data.FromJSON CodeContentDescription where
  parseJSON =
    Data.withObject
      "CodeContentDescription"
      ( \x ->
          CodeContentDescription'
            Prelude.<$> (x Data..:? "TextContent")
            Prelude.<*> (x Data..:? "CodeMD5")
            Prelude.<*> (x Data..:? "S3ApplicationCodeLocationDescription")
            Prelude.<*> (x Data..:? "CodeSize")
      )

instance Prelude.Hashable CodeContentDescription where
  hashWithSalt _salt CodeContentDescription' {..} =
    _salt `Prelude.hashWithSalt` textContent
      `Prelude.hashWithSalt` codeMD5
      `Prelude.hashWithSalt` s3ApplicationCodeLocationDescription
      `Prelude.hashWithSalt` codeSize

instance Prelude.NFData CodeContentDescription where
  rnf CodeContentDescription' {..} =
    Prelude.rnf textContent
      `Prelude.seq` Prelude.rnf codeMD5
      `Prelude.seq` Prelude.rnf s3ApplicationCodeLocationDescription
      `Prelude.seq` Prelude.rnf codeSize
