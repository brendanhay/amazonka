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
-- Module      : Amazonka.CloudFront.Types.OriginCustomHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginCustomHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains @HeaderName@ and @HeaderValue@ elements, if
-- any, for this distribution.
--
-- /See:/ 'newOriginCustomHeader' smart constructor.
data OriginCustomHeader = OriginCustomHeader'
  { -- | The name of a header that you want CloudFront to send to your origin.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/forward-custom-headers.html Adding Custom Headers to Origin Requests>
    -- in the /Amazon CloudFront Developer Guide/.
    headerName :: Prelude.Text,
    -- | The value for the header that you specified in the @HeaderName@ field.
    headerValue :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginCustomHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headerName', 'originCustomHeader_headerName' - The name of a header that you want CloudFront to send to your origin.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/forward-custom-headers.html Adding Custom Headers to Origin Requests>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'headerValue', 'originCustomHeader_headerValue' - The value for the header that you specified in the @HeaderName@ field.
newOriginCustomHeader ::
  -- | 'headerName'
  Prelude.Text ->
  -- | 'headerValue'
  Prelude.Text ->
  OriginCustomHeader
newOriginCustomHeader pHeaderName_ pHeaderValue_ =
  OriginCustomHeader'
    { headerName = pHeaderName_,
      headerValue = Data._Sensitive Lens.# pHeaderValue_
    }

-- | The name of a header that you want CloudFront to send to your origin.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/forward-custom-headers.html Adding Custom Headers to Origin Requests>
-- in the /Amazon CloudFront Developer Guide/.
originCustomHeader_headerName :: Lens.Lens' OriginCustomHeader Prelude.Text
originCustomHeader_headerName = Lens.lens (\OriginCustomHeader' {headerName} -> headerName) (\s@OriginCustomHeader' {} a -> s {headerName = a} :: OriginCustomHeader)

-- | The value for the header that you specified in the @HeaderName@ field.
originCustomHeader_headerValue :: Lens.Lens' OriginCustomHeader Prelude.Text
originCustomHeader_headerValue = Lens.lens (\OriginCustomHeader' {headerValue} -> headerValue) (\s@OriginCustomHeader' {} a -> s {headerValue = a} :: OriginCustomHeader) Prelude.. Data._Sensitive

instance Data.FromXML OriginCustomHeader where
  parseXML x =
    OriginCustomHeader'
      Prelude.<$> (x Data..@ "HeaderName")
      Prelude.<*> (x Data..@ "HeaderValue")

instance Prelude.Hashable OriginCustomHeader where
  hashWithSalt _salt OriginCustomHeader' {..} =
    _salt
      `Prelude.hashWithSalt` headerName
      `Prelude.hashWithSalt` headerValue

instance Prelude.NFData OriginCustomHeader where
  rnf OriginCustomHeader' {..} =
    Prelude.rnf headerName `Prelude.seq`
      Prelude.rnf headerValue

instance Data.ToXML OriginCustomHeader where
  toXML OriginCustomHeader' {..} =
    Prelude.mconcat
      [ "HeaderName" Data.@= headerName,
        "HeaderValue" Data.@= headerValue
      ]
