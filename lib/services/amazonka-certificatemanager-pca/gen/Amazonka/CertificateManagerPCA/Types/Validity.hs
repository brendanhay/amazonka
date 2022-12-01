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
-- Module      : Amazonka.CertificateManagerPCA.Types.Validity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.Validity where

import Amazonka.CertificateManagerPCA.Types.ValidityPeriodType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Validity specifies the period of time during which a certificate is
-- valid. Validity can be expressed as an explicit date and time when the
-- validity of a certificate starts or expires, or as a span of time after
-- issuance, stated in days, months, or years. For more information, see
-- <https://tools.ietf.org/html/rfc5280#section-4.1.2.5 Validity> in RFC
-- 5280.
--
-- ACM Private CA API consumes the @Validity@ data type differently in two
-- distinct parameters of the @IssueCertificate@ action. The required
-- parameter @IssueCertificate@:@Validity@ specifies the end of a
-- certificate\'s validity period. The optional parameter
-- @IssueCertificate@:@ValidityNotBefore@ specifies a customized starting
-- time for the validity period.
--
-- /See:/ 'newValidity' smart constructor.
data Validity = Validity'
  { -- | A long integer interpreted according to the value of @Type@, below.
    value :: Prelude.Natural,
    -- | Determines how /ACM Private CA/ interprets the @Value@ parameter, an
    -- integer. Supported validity types include those listed below. Type
    -- definitions with values include a sample input value and the resulting
    -- output.
    --
    -- @END_DATE@: The specific date and time when the certificate will expire,
    -- expressed using UTCTime (YYMMDDHHMMSS) or GeneralizedTime
    -- (YYYYMMDDHHMMSS) format. When UTCTime is used, if the year field (YY) is
    -- greater than or equal to 50, the year is interpreted as 19YY. If the
    -- year field is less than 50, the year is interpreted as 20YY.
    --
    -- -   Sample input value: 491231235959 (UTCTime format)
    --
    -- -   Output expiration date\/time: 12\/31\/2049 23:59:59
    --
    -- @ABSOLUTE@: The specific date and time when the validity of a
    -- certificate will start or expire, expressed in seconds since the Unix
    -- Epoch.
    --
    -- -   Sample input value: 2524608000
    --
    -- -   Output expiration date\/time: 01\/01\/2050 00:00:00
    --
    -- @DAYS@, @MONTHS@, @YEARS@: The relative time from the moment of issuance
    -- until the certificate will expire, expressed in days, months, or years.
    --
    -- Example if @DAYS@, issued on 10\/12\/2020 at 12:34:54 UTC:
    --
    -- -   Sample input value: 90
    --
    -- -   Output expiration date: 01\/10\/2020 12:34:54 UTC
    --
    -- The minimum validity duration for a certificate using relative time
    -- (@DAYS@) is one day. The minimum validity for a certificate using
    -- absolute time (@ABSOLUTE@ or @END_DATE@) is one second.
    type' :: ValidityPeriodType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Validity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'validity_value' - A long integer interpreted according to the value of @Type@, below.
--
-- 'type'', 'validity_type' - Determines how /ACM Private CA/ interprets the @Value@ parameter, an
-- integer. Supported validity types include those listed below. Type
-- definitions with values include a sample input value and the resulting
-- output.
--
-- @END_DATE@: The specific date and time when the certificate will expire,
-- expressed using UTCTime (YYMMDDHHMMSS) or GeneralizedTime
-- (YYYYMMDDHHMMSS) format. When UTCTime is used, if the year field (YY) is
-- greater than or equal to 50, the year is interpreted as 19YY. If the
-- year field is less than 50, the year is interpreted as 20YY.
--
-- -   Sample input value: 491231235959 (UTCTime format)
--
-- -   Output expiration date\/time: 12\/31\/2049 23:59:59
--
-- @ABSOLUTE@: The specific date and time when the validity of a
-- certificate will start or expire, expressed in seconds since the Unix
-- Epoch.
--
-- -   Sample input value: 2524608000
--
-- -   Output expiration date\/time: 01\/01\/2050 00:00:00
--
-- @DAYS@, @MONTHS@, @YEARS@: The relative time from the moment of issuance
-- until the certificate will expire, expressed in days, months, or years.
--
-- Example if @DAYS@, issued on 10\/12\/2020 at 12:34:54 UTC:
--
-- -   Sample input value: 90
--
-- -   Output expiration date: 01\/10\/2020 12:34:54 UTC
--
-- The minimum validity duration for a certificate using relative time
-- (@DAYS@) is one day. The minimum validity for a certificate using
-- absolute time (@ABSOLUTE@ or @END_DATE@) is one second.
newValidity ::
  -- | 'value'
  Prelude.Natural ->
  -- | 'type''
  ValidityPeriodType ->
  Validity
newValidity pValue_ pType_ =
  Validity' {value = pValue_, type' = pType_}

-- | A long integer interpreted according to the value of @Type@, below.
validity_value :: Lens.Lens' Validity Prelude.Natural
validity_value = Lens.lens (\Validity' {value} -> value) (\s@Validity' {} a -> s {value = a} :: Validity)

-- | Determines how /ACM Private CA/ interprets the @Value@ parameter, an
-- integer. Supported validity types include those listed below. Type
-- definitions with values include a sample input value and the resulting
-- output.
--
-- @END_DATE@: The specific date and time when the certificate will expire,
-- expressed using UTCTime (YYMMDDHHMMSS) or GeneralizedTime
-- (YYYYMMDDHHMMSS) format. When UTCTime is used, if the year field (YY) is
-- greater than or equal to 50, the year is interpreted as 19YY. If the
-- year field is less than 50, the year is interpreted as 20YY.
--
-- -   Sample input value: 491231235959 (UTCTime format)
--
-- -   Output expiration date\/time: 12\/31\/2049 23:59:59
--
-- @ABSOLUTE@: The specific date and time when the validity of a
-- certificate will start or expire, expressed in seconds since the Unix
-- Epoch.
--
-- -   Sample input value: 2524608000
--
-- -   Output expiration date\/time: 01\/01\/2050 00:00:00
--
-- @DAYS@, @MONTHS@, @YEARS@: The relative time from the moment of issuance
-- until the certificate will expire, expressed in days, months, or years.
--
-- Example if @DAYS@, issued on 10\/12\/2020 at 12:34:54 UTC:
--
-- -   Sample input value: 90
--
-- -   Output expiration date: 01\/10\/2020 12:34:54 UTC
--
-- The minimum validity duration for a certificate using relative time
-- (@DAYS@) is one day. The minimum validity for a certificate using
-- absolute time (@ABSOLUTE@ or @END_DATE@) is one second.
validity_type :: Lens.Lens' Validity ValidityPeriodType
validity_type = Lens.lens (\Validity' {type'} -> type') (\s@Validity' {} a -> s {type' = a} :: Validity)

instance Prelude.Hashable Validity where
  hashWithSalt _salt Validity' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Validity where
  rnf Validity' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON Validity where
  toJSON Validity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Value" Core..= value),
            Prelude.Just ("Type" Core..= type')
          ]
      )
