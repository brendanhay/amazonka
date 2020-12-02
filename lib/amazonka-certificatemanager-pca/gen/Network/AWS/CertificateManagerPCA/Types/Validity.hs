{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.Validity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.Validity where

import Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Validity specifies the period of time during which a certificate is valid. Validity can be expressed as an explicit date and time when the certificate expires, or as a span of time after issuance, stated in days, months, or years. For more information, see <https://tools.ietf.org/html/rfc5280#section-4.1.2.5 Validity> in RFC 5280.
--
--
-- You can issue a certificate by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_IssueCertificate.html IssueCertificate> action.
--
--
-- /See:/ 'validity' smart constructor.
data Validity = Validity'
  { _vValue :: !Nat,
    _vType :: !ValidityPeriodType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Validity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vValue' - A long integer interpreted according to the value of @Type@ , below.
--
-- * 'vType' - Determines how /ACM Private CA/ interprets the @Value@ parameter, an integer. Supported validity types include those listed below. Type definitions with values include a sample input value and the resulting output.  @END_DATE@ : The specific date and time when the certificate will expire, expressed using UTCTime (YYMMDDHHMMSS) or GeneralizedTime (YYYYMMDDHHMMSS) format. When UTCTime is used, if the year field (YY) is greater than or equal to 50, the year is interpreted as 19YY. If the year field is less than 50, the year is interpreted as 20YY.     * Sample input value: 491231235959 (UTCTime format)     * Output expiration date/time: 12/31/2049 23:59:59 @ABSOLUTE@ : The specific date and time when the certificate will expire, expressed in seconds since the Unix Epoch.      * Sample input value: 2524608000     * Output expiration date/time: 01/01/2050 00:00:00 @DAYS@ , @MONTHS@ , @YEARS@ : The relative time from the moment of issuance until the certificate will expire, expressed in days, months, or years.  Example if @DAYS@ , issued on 10/12/2020 at 12:34:54 UTC:     * Sample input value: 90     * Output expiration date: 01/10/2020 12:34:54 UTC
validity ::
  -- | 'vValue'
  Natural ->
  -- | 'vType'
  ValidityPeriodType ->
  Validity
validity pValue_ pType_ =
  Validity' {_vValue = _Nat # pValue_, _vType = pType_}

-- | A long integer interpreted according to the value of @Type@ , below.
vValue :: Lens' Validity Natural
vValue = lens _vValue (\s a -> s {_vValue = a}) . _Nat

-- | Determines how /ACM Private CA/ interprets the @Value@ parameter, an integer. Supported validity types include those listed below. Type definitions with values include a sample input value and the resulting output.  @END_DATE@ : The specific date and time when the certificate will expire, expressed using UTCTime (YYMMDDHHMMSS) or GeneralizedTime (YYYYMMDDHHMMSS) format. When UTCTime is used, if the year field (YY) is greater than or equal to 50, the year is interpreted as 19YY. If the year field is less than 50, the year is interpreted as 20YY.     * Sample input value: 491231235959 (UTCTime format)     * Output expiration date/time: 12/31/2049 23:59:59 @ABSOLUTE@ : The specific date and time when the certificate will expire, expressed in seconds since the Unix Epoch.      * Sample input value: 2524608000     * Output expiration date/time: 01/01/2050 00:00:00 @DAYS@ , @MONTHS@ , @YEARS@ : The relative time from the moment of issuance until the certificate will expire, expressed in days, months, or years.  Example if @DAYS@ , issued on 10/12/2020 at 12:34:54 UTC:     * Sample input value: 90     * Output expiration date: 01/10/2020 12:34:54 UTC
vType :: Lens' Validity ValidityPeriodType
vType = lens _vType (\s a -> s {_vType = a})

instance Hashable Validity

instance NFData Validity

instance ToJSON Validity where
  toJSON Validity' {..} =
    object
      (catMaybes [Just ("Value" .= _vValue), Just ("Type" .= _vType)])
