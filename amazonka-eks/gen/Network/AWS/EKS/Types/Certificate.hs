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
-- Module      : Network.AWS.EKS.Types.Certificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.Certificate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing the @certificate-authority-data@ for your
-- cluster.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The Base64-encoded certificate data required to communicate with your
    -- cluster. Add this to the @certificate-authority-data@ section of the
    -- @kubeconfig@ file for your cluster.
    data' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
newCertificate = Certificate' {data' = Core.Nothing}

-- | The Base64-encoded certificate data required to communicate with your
-- cluster. Add this to the @certificate-authority-data@ section of the
-- @kubeconfig@ file for your cluster.
certificate_data :: Lens.Lens' Certificate (Core.Maybe Core.Text)
certificate_data = Lens.lens (\Certificate' {data'} -> data') (\s@Certificate' {} a -> s {data' = a} :: Certificate)

instance Core.FromJSON Certificate where
  parseJSON =
    Core.withObject
      "Certificate"
      (\x -> Certificate' Core.<$> (x Core..:? "data"))

instance Core.Hashable Certificate

instance Core.NFData Certificate
