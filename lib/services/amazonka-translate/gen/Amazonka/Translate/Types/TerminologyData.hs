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
-- Module      : Amazonka.Translate.Types.TerminologyData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.TerminologyData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Translate.Types.Directionality
import Amazonka.Translate.Types.TerminologyDataFormat

-- | The data associated with the custom terminology. For information about
-- the custom terminology file, see
-- <https://docs.aws.amazon.com/translate/latest/dg/creating-custom-terminology.html Creating a Custom Terminology>.
--
-- /See:/ 'newTerminologyData' smart constructor.
data TerminologyData = TerminologyData'
  { -- | The directionality of your terminology resource indicates whether it has
    -- one source language (uni-directional) or multiple (multi-directional).
    --
    -- [UNI]
    --     The terminology resource has one source language (for example, the
    --     first column in a CSV file), and all of its other languages are
    --     target languages.
    --
    -- [MULTI]
    --     Any language in the terminology resource can be the source language
    --     or a target language. A single multi-directional terminology
    --     resource can be used for jobs that translate different language
    --     pairs. For example, if the terminology contains English and Spanish
    --     terms, it can be used for jobs that translate English to Spanish and
    --     Spanish to English.
    --
    -- When you create a custom terminology resource without specifying the
    -- directionality, it behaves as uni-directional terminology, although this
    -- parameter will have a null value.
    directionality :: Prelude.Maybe Directionality,
    -- | The file containing the custom terminology data. Your version of the AWS
    -- SDK performs a Base64-encoding on this field before sending a request to
    -- the AWS service. Users of the SDK should not perform Base64-encoding
    -- themselves.
    file :: Data.Sensitive Data.Base64,
    -- | The data format of the custom terminology.
    format :: TerminologyDataFormat
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminologyData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directionality', 'terminologyData_directionality' - The directionality of your terminology resource indicates whether it has
-- one source language (uni-directional) or multiple (multi-directional).
--
-- [UNI]
--     The terminology resource has one source language (for example, the
--     first column in a CSV file), and all of its other languages are
--     target languages.
--
-- [MULTI]
--     Any language in the terminology resource can be the source language
--     or a target language. A single multi-directional terminology
--     resource can be used for jobs that translate different language
--     pairs. For example, if the terminology contains English and Spanish
--     terms, it can be used for jobs that translate English to Spanish and
--     Spanish to English.
--
-- When you create a custom terminology resource without specifying the
-- directionality, it behaves as uni-directional terminology, although this
-- parameter will have a null value.
--
-- 'file', 'terminologyData_file' - The file containing the custom terminology data. Your version of the AWS
-- SDK performs a Base64-encoding on this field before sending a request to
-- the AWS service. Users of the SDK should not perform Base64-encoding
-- themselves.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'format', 'terminologyData_format' - The data format of the custom terminology.
newTerminologyData ::
  -- | 'file'
  Prelude.ByteString ->
  -- | 'format'
  TerminologyDataFormat ->
  TerminologyData
newTerminologyData pFile_ pFormat_ =
  TerminologyData'
    { directionality = Prelude.Nothing,
      file =
        Data._Sensitive Prelude.. Data._Base64 Lens.# pFile_,
      format = pFormat_
    }

-- | The directionality of your terminology resource indicates whether it has
-- one source language (uni-directional) or multiple (multi-directional).
--
-- [UNI]
--     The terminology resource has one source language (for example, the
--     first column in a CSV file), and all of its other languages are
--     target languages.
--
-- [MULTI]
--     Any language in the terminology resource can be the source language
--     or a target language. A single multi-directional terminology
--     resource can be used for jobs that translate different language
--     pairs. For example, if the terminology contains English and Spanish
--     terms, it can be used for jobs that translate English to Spanish and
--     Spanish to English.
--
-- When you create a custom terminology resource without specifying the
-- directionality, it behaves as uni-directional terminology, although this
-- parameter will have a null value.
terminologyData_directionality :: Lens.Lens' TerminologyData (Prelude.Maybe Directionality)
terminologyData_directionality = Lens.lens (\TerminologyData' {directionality} -> directionality) (\s@TerminologyData' {} a -> s {directionality = a} :: TerminologyData)

-- | The file containing the custom terminology data. Your version of the AWS
-- SDK performs a Base64-encoding on this field before sending a request to
-- the AWS service. Users of the SDK should not perform Base64-encoding
-- themselves.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
terminologyData_file :: Lens.Lens' TerminologyData Prelude.ByteString
terminologyData_file = Lens.lens (\TerminologyData' {file} -> file) (\s@TerminologyData' {} a -> s {file = a} :: TerminologyData) Prelude.. Data._Sensitive Prelude.. Data._Base64

-- | The data format of the custom terminology.
terminologyData_format :: Lens.Lens' TerminologyData TerminologyDataFormat
terminologyData_format = Lens.lens (\TerminologyData' {format} -> format) (\s@TerminologyData' {} a -> s {format = a} :: TerminologyData)

instance Prelude.Hashable TerminologyData where
  hashWithSalt _salt TerminologyData' {..} =
    _salt
      `Prelude.hashWithSalt` directionality
      `Prelude.hashWithSalt` file
      `Prelude.hashWithSalt` format

instance Prelude.NFData TerminologyData where
  rnf TerminologyData' {..} =
    Prelude.rnf directionality
      `Prelude.seq` Prelude.rnf file
      `Prelude.seq` Prelude.rnf format

instance Data.ToJSON TerminologyData where
  toJSON TerminologyData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Directionality" Data..=)
              Prelude.<$> directionality,
            Prelude.Just ("File" Data..= file),
            Prelude.Just ("Format" Data..= format)
          ]
      )
