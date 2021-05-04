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
-- Module      : Network.AWS.Organizations.Types.EnabledServicePrincipal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.EnabledServicePrincipal where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure that contains details of a service principal that represents
-- an AWS service that is enabled to integrate with AWS Organizations.
--
-- /See:/ 'newEnabledServicePrincipal' smart constructor.
data EnabledServicePrincipal = EnabledServicePrincipal'
  { -- | The name of the service principal. This is typically in the form of a
    -- URL, such as: @ servicename.amazonaws.com@.
    servicePrincipal :: Prelude.Maybe Prelude.Text,
    -- | The date that the service principal was enabled for integration with AWS
    -- Organizations.
    dateEnabled :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnabledServicePrincipal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'servicePrincipal', 'enabledServicePrincipal_servicePrincipal' - The name of the service principal. This is typically in the form of a
-- URL, such as: @ servicename.amazonaws.com@.
--
-- 'dateEnabled', 'enabledServicePrincipal_dateEnabled' - The date that the service principal was enabled for integration with AWS
-- Organizations.
newEnabledServicePrincipal ::
  EnabledServicePrincipal
newEnabledServicePrincipal =
  EnabledServicePrincipal'
    { servicePrincipal =
        Prelude.Nothing,
      dateEnabled = Prelude.Nothing
    }

-- | The name of the service principal. This is typically in the form of a
-- URL, such as: @ servicename.amazonaws.com@.
enabledServicePrincipal_servicePrincipal :: Lens.Lens' EnabledServicePrincipal (Prelude.Maybe Prelude.Text)
enabledServicePrincipal_servicePrincipal = Lens.lens (\EnabledServicePrincipal' {servicePrincipal} -> servicePrincipal) (\s@EnabledServicePrincipal' {} a -> s {servicePrincipal = a} :: EnabledServicePrincipal)

-- | The date that the service principal was enabled for integration with AWS
-- Organizations.
enabledServicePrincipal_dateEnabled :: Lens.Lens' EnabledServicePrincipal (Prelude.Maybe Prelude.UTCTime)
enabledServicePrincipal_dateEnabled = Lens.lens (\EnabledServicePrincipal' {dateEnabled} -> dateEnabled) (\s@EnabledServicePrincipal' {} a -> s {dateEnabled = a} :: EnabledServicePrincipal) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON EnabledServicePrincipal where
  parseJSON =
    Prelude.withObject
      "EnabledServicePrincipal"
      ( \x ->
          EnabledServicePrincipal'
            Prelude.<$> (x Prelude..:? "ServicePrincipal")
            Prelude.<*> (x Prelude..:? "DateEnabled")
      )

instance Prelude.Hashable EnabledServicePrincipal

instance Prelude.NFData EnabledServicePrincipal
