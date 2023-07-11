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
-- Module      : Amazonka.EKS.Types.Certificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.Certificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the @certificate-authority-data@ for your
-- cluster.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The Base64-encoded certificate data required to communicate with your
    -- cluster. Add this to the @certificate-authority-data@ section of the
    -- @kubeconfig@ file for your cluster.
    data' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'certificate_data' - The Base64-encoded certificate data required to communicate with your
-- cluster. Add this to the @certificate-authority-data@ section of the
-- @kubeconfig@ file for your cluster.
newCertificate ::
  Certificate
newCertificate =
  Certificate' {data' = Prelude.Nothing}

-- | The Base64-encoded certificate data required to communicate with your
-- cluster. Add this to the @certificate-authority-data@ section of the
-- @kubeconfig@ file for your cluster.
certificate_data :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_data = Lens.lens (\Certificate' {data'} -> data') (\s@Certificate' {} a -> s {data' = a} :: Certificate)

instance Data.FromJSON Certificate where
  parseJSON =
    Data.withObject
      "Certificate"
      (\x -> Certificate' Prelude.<$> (x Data..:? "data"))

instance Prelude.Hashable Certificate where
  hashWithSalt _salt Certificate' {..} =
    _salt `Prelude.hashWithSalt` data'

instance Prelude.NFData Certificate where
  rnf Certificate' {..} = Prelude.rnf data'
