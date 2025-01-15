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
-- Module      : Amazonka.RDSData.Types.ResultSetOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDSData.Types.ResultSetOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDSData.Types.DecimalReturnType
import Amazonka.RDSData.Types.LongReturnType

-- | Options that control how the result set is returned.
--
-- /See:/ 'newResultSetOptions' smart constructor.
data ResultSetOptions = ResultSetOptions'
  { -- | A value that indicates how a field of @DECIMAL@ type is represented in
    -- the response. The value of @STRING@, the default, specifies that it is
    -- converted to a String value. The value of @DOUBLE_OR_LONG@ specifies
    -- that it is converted to a Long value if its scale is 0, or to a Double
    -- value otherwise.
    --
    -- Conversion to Double or Long can result in roundoff errors due to
    -- precision loss. We recommend converting to String, especially when
    -- working with currency values.
    decimalReturnType :: Prelude.Maybe DecimalReturnType,
    -- | A value that indicates how a field of @LONG@ type is represented.
    -- Allowed values are @LONG@ and @STRING@. The default is @LONG@. Specify
    -- @STRING@ if the length or precision of numeric values might cause
    -- truncation or rounding errors.
    longReturnType :: Prelude.Maybe LongReturnType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResultSetOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decimalReturnType', 'resultSetOptions_decimalReturnType' - A value that indicates how a field of @DECIMAL@ type is represented in
-- the response. The value of @STRING@, the default, specifies that it is
-- converted to a String value. The value of @DOUBLE_OR_LONG@ specifies
-- that it is converted to a Long value if its scale is 0, or to a Double
-- value otherwise.
--
-- Conversion to Double or Long can result in roundoff errors due to
-- precision loss. We recommend converting to String, especially when
-- working with currency values.
--
-- 'longReturnType', 'resultSetOptions_longReturnType' - A value that indicates how a field of @LONG@ type is represented.
-- Allowed values are @LONG@ and @STRING@. The default is @LONG@. Specify
-- @STRING@ if the length or precision of numeric values might cause
-- truncation or rounding errors.
newResultSetOptions ::
  ResultSetOptions
newResultSetOptions =
  ResultSetOptions'
    { decimalReturnType =
        Prelude.Nothing,
      longReturnType = Prelude.Nothing
    }

-- | A value that indicates how a field of @DECIMAL@ type is represented in
-- the response. The value of @STRING@, the default, specifies that it is
-- converted to a String value. The value of @DOUBLE_OR_LONG@ specifies
-- that it is converted to a Long value if its scale is 0, or to a Double
-- value otherwise.
--
-- Conversion to Double or Long can result in roundoff errors due to
-- precision loss. We recommend converting to String, especially when
-- working with currency values.
resultSetOptions_decimalReturnType :: Lens.Lens' ResultSetOptions (Prelude.Maybe DecimalReturnType)
resultSetOptions_decimalReturnType = Lens.lens (\ResultSetOptions' {decimalReturnType} -> decimalReturnType) (\s@ResultSetOptions' {} a -> s {decimalReturnType = a} :: ResultSetOptions)

-- | A value that indicates how a field of @LONG@ type is represented.
-- Allowed values are @LONG@ and @STRING@. The default is @LONG@. Specify
-- @STRING@ if the length or precision of numeric values might cause
-- truncation or rounding errors.
resultSetOptions_longReturnType :: Lens.Lens' ResultSetOptions (Prelude.Maybe LongReturnType)
resultSetOptions_longReturnType = Lens.lens (\ResultSetOptions' {longReturnType} -> longReturnType) (\s@ResultSetOptions' {} a -> s {longReturnType = a} :: ResultSetOptions)

instance Prelude.Hashable ResultSetOptions where
  hashWithSalt _salt ResultSetOptions' {..} =
    _salt
      `Prelude.hashWithSalt` decimalReturnType
      `Prelude.hashWithSalt` longReturnType

instance Prelude.NFData ResultSetOptions where
  rnf ResultSetOptions' {..} =
    Prelude.rnf decimalReturnType `Prelude.seq`
      Prelude.rnf longReturnType

instance Data.ToJSON ResultSetOptions where
  toJSON ResultSetOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("decimalReturnType" Data..=)
              Prelude.<$> decimalReturnType,
            ("longReturnType" Data..=)
              Prelude.<$> longReturnType
          ]
      )
