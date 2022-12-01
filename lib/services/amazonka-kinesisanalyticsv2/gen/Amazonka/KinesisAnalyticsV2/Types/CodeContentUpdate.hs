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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.CodeContentUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.CodeContentUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types.S3ContentLocationUpdate
import qualified Amazonka.Prelude as Prelude

-- | Describes an update to the code of an application. Not supported for
-- Apache Zeppelin.
--
-- /See:/ 'newCodeContentUpdate' smart constructor.
data CodeContentUpdate = CodeContentUpdate'
  { -- | Describes an update to the location of code for an application.
    s3ContentLocationUpdate :: Prelude.Maybe S3ContentLocationUpdate,
    -- | Describes an update to the text code for an application.
    textContentUpdate :: Prelude.Maybe Prelude.Text,
    -- | Describes an update to the zipped code for an application.
    zipFileContentUpdate :: Prelude.Maybe Core.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeContentUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ContentLocationUpdate', 'codeContentUpdate_s3ContentLocationUpdate' - Describes an update to the location of code for an application.
--
-- 'textContentUpdate', 'codeContentUpdate_textContentUpdate' - Describes an update to the text code for an application.
--
-- 'zipFileContentUpdate', 'codeContentUpdate_zipFileContentUpdate' - Describes an update to the zipped code for an application.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newCodeContentUpdate ::
  CodeContentUpdate
newCodeContentUpdate =
  CodeContentUpdate'
    { s3ContentLocationUpdate =
        Prelude.Nothing,
      textContentUpdate = Prelude.Nothing,
      zipFileContentUpdate = Prelude.Nothing
    }

-- | Describes an update to the location of code for an application.
codeContentUpdate_s3ContentLocationUpdate :: Lens.Lens' CodeContentUpdate (Prelude.Maybe S3ContentLocationUpdate)
codeContentUpdate_s3ContentLocationUpdate = Lens.lens (\CodeContentUpdate' {s3ContentLocationUpdate} -> s3ContentLocationUpdate) (\s@CodeContentUpdate' {} a -> s {s3ContentLocationUpdate = a} :: CodeContentUpdate)

-- | Describes an update to the text code for an application.
codeContentUpdate_textContentUpdate :: Lens.Lens' CodeContentUpdate (Prelude.Maybe Prelude.Text)
codeContentUpdate_textContentUpdate = Lens.lens (\CodeContentUpdate' {textContentUpdate} -> textContentUpdate) (\s@CodeContentUpdate' {} a -> s {textContentUpdate = a} :: CodeContentUpdate)

-- | Describes an update to the zipped code for an application.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
codeContentUpdate_zipFileContentUpdate :: Lens.Lens' CodeContentUpdate (Prelude.Maybe Prelude.ByteString)
codeContentUpdate_zipFileContentUpdate = Lens.lens (\CodeContentUpdate' {zipFileContentUpdate} -> zipFileContentUpdate) (\s@CodeContentUpdate' {} a -> s {zipFileContentUpdate = a} :: CodeContentUpdate) Prelude.. Lens.mapping Core._Base64

instance Prelude.Hashable CodeContentUpdate where
  hashWithSalt _salt CodeContentUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` s3ContentLocationUpdate
      `Prelude.hashWithSalt` textContentUpdate
      `Prelude.hashWithSalt` zipFileContentUpdate

instance Prelude.NFData CodeContentUpdate where
  rnf CodeContentUpdate' {..} =
    Prelude.rnf s3ContentLocationUpdate
      `Prelude.seq` Prelude.rnf textContentUpdate
      `Prelude.seq` Prelude.rnf zipFileContentUpdate

instance Core.ToJSON CodeContentUpdate where
  toJSON CodeContentUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3ContentLocationUpdate" Core..=)
              Prelude.<$> s3ContentLocationUpdate,
            ("TextContentUpdate" Core..=)
              Prelude.<$> textContentUpdate,
            ("ZipFileContentUpdate" Core..=)
              Prelude.<$> zipFileContentUpdate
          ]
      )
