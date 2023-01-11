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
-- Module      : Amazonka.Firehose.Types.Record
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.Record where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The unit of data in a delivery stream.
--
-- /See:/ 'newRecord' smart constructor.
data Record = Record'
  { -- | The data blob, which is base64-encoded when the blob is serialized. The
    -- maximum size of the data blob, before base64-encoding, is 1,000 KiB.
    data' :: Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Record' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'record_data' - The data blob, which is base64-encoded when the blob is serialized. The
-- maximum size of the data blob, before base64-encoding, is 1,000 KiB.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newRecord ::
  -- | 'data''
  Prelude.ByteString ->
  Record
newRecord pData_ =
  Record' {data' = Data._Base64 Lens.# pData_}

-- | The data blob, which is base64-encoded when the blob is serialized. The
-- maximum size of the data blob, before base64-encoding, is 1,000 KiB.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
record_data :: Lens.Lens' Record Prelude.ByteString
record_data = Lens.lens (\Record' {data'} -> data') (\s@Record' {} a -> s {data' = a} :: Record) Prelude.. Data._Base64

instance Prelude.Hashable Record where
  hashWithSalt _salt Record' {..} =
    _salt `Prelude.hashWithSalt` data'

instance Prelude.NFData Record where
  rnf Record' {..} = Prelude.rnf data'

instance Data.ToJSON Record where
  toJSON Record' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Data" Data..= data')]
      )
