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
-- Module      : Amazonka.CloudFront.Types.ContinuousDeploymentSingleHeaderConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ContinuousDeploymentSingleHeaderConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This configuration determines which HTTP requests are sent to the
-- staging distribution. If the HTTP request contains a header and value
-- that matches what you specify here, the request is sent to the staging
-- distribution. Otherwise the request is sent to the primary distribution.
--
-- /See:/ 'newContinuousDeploymentSingleHeaderConfig' smart constructor.
data ContinuousDeploymentSingleHeaderConfig = ContinuousDeploymentSingleHeaderConfig'
  { -- | The request header name that you want CloudFront to send to your staging
    -- distribution.
    header :: Prelude.Text,
    -- | The request header value.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContinuousDeploymentSingleHeaderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'header', 'continuousDeploymentSingleHeaderConfig_header' - The request header name that you want CloudFront to send to your staging
-- distribution.
--
-- 'value', 'continuousDeploymentSingleHeaderConfig_value' - The request header value.
newContinuousDeploymentSingleHeaderConfig ::
  -- | 'header'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  ContinuousDeploymentSingleHeaderConfig
newContinuousDeploymentSingleHeaderConfig
  pHeader_
  pValue_ =
    ContinuousDeploymentSingleHeaderConfig'
      { header =
          pHeader_,
        value = pValue_
      }

-- | The request header name that you want CloudFront to send to your staging
-- distribution.
continuousDeploymentSingleHeaderConfig_header :: Lens.Lens' ContinuousDeploymentSingleHeaderConfig Prelude.Text
continuousDeploymentSingleHeaderConfig_header = Lens.lens (\ContinuousDeploymentSingleHeaderConfig' {header} -> header) (\s@ContinuousDeploymentSingleHeaderConfig' {} a -> s {header = a} :: ContinuousDeploymentSingleHeaderConfig)

-- | The request header value.
continuousDeploymentSingleHeaderConfig_value :: Lens.Lens' ContinuousDeploymentSingleHeaderConfig Prelude.Text
continuousDeploymentSingleHeaderConfig_value = Lens.lens (\ContinuousDeploymentSingleHeaderConfig' {value} -> value) (\s@ContinuousDeploymentSingleHeaderConfig' {} a -> s {value = a} :: ContinuousDeploymentSingleHeaderConfig)

instance
  Data.FromXML
    ContinuousDeploymentSingleHeaderConfig
  where
  parseXML x =
    ContinuousDeploymentSingleHeaderConfig'
      Prelude.<$> (x Data..@ "Header") Prelude.<*> (x Data..@ "Value")

instance
  Prelude.Hashable
    ContinuousDeploymentSingleHeaderConfig
  where
  hashWithSalt
    _salt
    ContinuousDeploymentSingleHeaderConfig' {..} =
      _salt `Prelude.hashWithSalt` header
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    ContinuousDeploymentSingleHeaderConfig
  where
  rnf ContinuousDeploymentSingleHeaderConfig' {..} =
    Prelude.rnf header `Prelude.seq` Prelude.rnf value

instance
  Data.ToXML
    ContinuousDeploymentSingleHeaderConfig
  where
  toXML ContinuousDeploymentSingleHeaderConfig' {..} =
    Prelude.mconcat
      ["Header" Data.@= header, "Value" Data.@= value]
