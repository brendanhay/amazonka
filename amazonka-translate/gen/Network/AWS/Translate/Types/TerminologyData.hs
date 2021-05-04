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
-- Module      : Network.AWS.Translate.Types.TerminologyData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TerminologyData where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Translate.Types.TerminologyDataFormat

-- | The data associated with the custom terminology.
--
-- /See:/ 'newTerminologyData' smart constructor.
data TerminologyData = TerminologyData'
  { -- | The file containing the custom terminology data. Your version of the AWS
    -- SDK performs a Base64-encoding on this field before sending a request to
    -- the AWS service. Users of the SDK should not perform Base64-encoding
    -- themselves.
    file :: Prelude.Sensitive Prelude.Base64,
    -- | The data format of the custom terminology. Either CSV or TMX.
    format :: TerminologyDataFormat
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TerminologyData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'format', 'terminologyData_format' - The data format of the custom terminology. Either CSV or TMX.
newTerminologyData ::
  -- | 'file'
  Prelude.ByteString ->
  -- | 'format'
  TerminologyDataFormat ->
  TerminologyData
newTerminologyData pFile_ pFormat_ =
  TerminologyData'
    { file =
        Prelude._Sensitive Prelude.. Prelude._Base64
          Lens.# pFile_,
      format = pFormat_
    }

-- | The file containing the custom terminology data. Your version of the AWS
-- SDK performs a Base64-encoding on this field before sending a request to
-- the AWS service. Users of the SDK should not perform Base64-encoding
-- themselves.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
terminologyData_file :: Lens.Lens' TerminologyData Prelude.ByteString
terminologyData_file = Lens.lens (\TerminologyData' {file} -> file) (\s@TerminologyData' {} a -> s {file = a} :: TerminologyData) Prelude.. Prelude._Sensitive Prelude.. Prelude._Base64

-- | The data format of the custom terminology. Either CSV or TMX.
terminologyData_format :: Lens.Lens' TerminologyData TerminologyDataFormat
terminologyData_format = Lens.lens (\TerminologyData' {format} -> format) (\s@TerminologyData' {} a -> s {format = a} :: TerminologyData)

instance Prelude.Hashable TerminologyData

instance Prelude.NFData TerminologyData

instance Prelude.ToJSON TerminologyData where
  toJSON TerminologyData' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("File" Prelude..= file),
            Prelude.Just ("Format" Prelude..= format)
          ]
      )
