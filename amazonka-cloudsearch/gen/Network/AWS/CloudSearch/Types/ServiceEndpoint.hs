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
-- Module      : Network.AWS.CloudSearch.Types.ServiceEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.ServiceEndpoint where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The endpoint to which service requests can be submitted.
--
-- /See:/ 'newServiceEndpoint' smart constructor.
data ServiceEndpoint = ServiceEndpoint'
  { endpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServiceEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'serviceEndpoint_endpoint' - Undocumented member.
newServiceEndpoint ::
  ServiceEndpoint
newServiceEndpoint =
  ServiceEndpoint' {endpoint = Prelude.Nothing}

-- | Undocumented member.
serviceEndpoint_endpoint :: Lens.Lens' ServiceEndpoint (Prelude.Maybe Prelude.Text)
serviceEndpoint_endpoint = Lens.lens (\ServiceEndpoint' {endpoint} -> endpoint) (\s@ServiceEndpoint' {} a -> s {endpoint = a} :: ServiceEndpoint)

instance Prelude.FromXML ServiceEndpoint where
  parseXML x =
    ServiceEndpoint'
      Prelude.<$> (x Prelude..@? "Endpoint")

instance Prelude.Hashable ServiceEndpoint

instance Prelude.NFData ServiceEndpoint
