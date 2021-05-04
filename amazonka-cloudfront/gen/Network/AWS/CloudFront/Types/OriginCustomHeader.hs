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
-- Module      : Network.AWS.CloudFront.Types.OriginCustomHeader
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginCustomHeader where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    headerValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
      headerValue = pHeaderValue_
    }

-- | The name of a header that you want CloudFront to send to your origin.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/forward-custom-headers.html Adding Custom Headers to Origin Requests>
-- in the /Amazon CloudFront Developer Guide/.
originCustomHeader_headerName :: Lens.Lens' OriginCustomHeader Prelude.Text
originCustomHeader_headerName = Lens.lens (\OriginCustomHeader' {headerName} -> headerName) (\s@OriginCustomHeader' {} a -> s {headerName = a} :: OriginCustomHeader)

-- | The value for the header that you specified in the @HeaderName@ field.
originCustomHeader_headerValue :: Lens.Lens' OriginCustomHeader Prelude.Text
originCustomHeader_headerValue = Lens.lens (\OriginCustomHeader' {headerValue} -> headerValue) (\s@OriginCustomHeader' {} a -> s {headerValue = a} :: OriginCustomHeader)

instance Prelude.FromXML OriginCustomHeader where
  parseXML x =
    OriginCustomHeader'
      Prelude.<$> (x Prelude..@ "HeaderName")
      Prelude.<*> (x Prelude..@ "HeaderValue")

instance Prelude.Hashable OriginCustomHeader

instance Prelude.NFData OriginCustomHeader

instance Prelude.ToXML OriginCustomHeader where
  toXML OriginCustomHeader' {..} =
    Prelude.mconcat
      [ "HeaderName" Prelude.@= headerName,
        "HeaderValue" Prelude.@= headerValue
      ]
