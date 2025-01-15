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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.CodeContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.CodeContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.S3ContentLocation
import qualified Amazonka.Prelude as Prelude

-- | Specifies either the application code, or the location of the
-- application code, for a Flink-based Kinesis Data Analytics application.
--
-- /See:/ 'newCodeContent' smart constructor.
data CodeContent = CodeContent'
  { -- | Information about the Amazon S3 bucket that contains the application
    -- code.
    s3ContentLocation :: Prelude.Maybe S3ContentLocation,
    -- | The text-format code for a Flink-based Kinesis Data Analytics
    -- application.
    textContent :: Prelude.Maybe Prelude.Text,
    -- | The zip-format code for a Flink-based Kinesis Data Analytics
    -- application.
    zipFileContent :: Prelude.Maybe Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ContentLocation', 'codeContent_s3ContentLocation' - Information about the Amazon S3 bucket that contains the application
-- code.
--
-- 'textContent', 'codeContent_textContent' - The text-format code for a Flink-based Kinesis Data Analytics
-- application.
--
-- 'zipFileContent', 'codeContent_zipFileContent' - The zip-format code for a Flink-based Kinesis Data Analytics
-- application.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newCodeContent ::
  CodeContent
newCodeContent =
  CodeContent'
    { s3ContentLocation = Prelude.Nothing,
      textContent = Prelude.Nothing,
      zipFileContent = Prelude.Nothing
    }

-- | Information about the Amazon S3 bucket that contains the application
-- code.
codeContent_s3ContentLocation :: Lens.Lens' CodeContent (Prelude.Maybe S3ContentLocation)
codeContent_s3ContentLocation = Lens.lens (\CodeContent' {s3ContentLocation} -> s3ContentLocation) (\s@CodeContent' {} a -> s {s3ContentLocation = a} :: CodeContent)

-- | The text-format code for a Flink-based Kinesis Data Analytics
-- application.
codeContent_textContent :: Lens.Lens' CodeContent (Prelude.Maybe Prelude.Text)
codeContent_textContent = Lens.lens (\CodeContent' {textContent} -> textContent) (\s@CodeContent' {} a -> s {textContent = a} :: CodeContent)

-- | The zip-format code for a Flink-based Kinesis Data Analytics
-- application.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
codeContent_zipFileContent :: Lens.Lens' CodeContent (Prelude.Maybe Prelude.ByteString)
codeContent_zipFileContent = Lens.lens (\CodeContent' {zipFileContent} -> zipFileContent) (\s@CodeContent' {} a -> s {zipFileContent = a} :: CodeContent) Prelude.. Lens.mapping Data._Base64

instance Prelude.Hashable CodeContent where
  hashWithSalt _salt CodeContent' {..} =
    _salt
      `Prelude.hashWithSalt` s3ContentLocation
      `Prelude.hashWithSalt` textContent
      `Prelude.hashWithSalt` zipFileContent

instance Prelude.NFData CodeContent where
  rnf CodeContent' {..} =
    Prelude.rnf s3ContentLocation `Prelude.seq`
      Prelude.rnf textContent `Prelude.seq`
        Prelude.rnf zipFileContent

instance Data.ToJSON CodeContent where
  toJSON CodeContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3ContentLocation" Data..=)
              Prelude.<$> s3ContentLocation,
            ("TextContent" Data..=) Prelude.<$> textContent,
            ("ZipFileContent" Data..=)
              Prelude.<$> zipFileContent
          ]
      )
