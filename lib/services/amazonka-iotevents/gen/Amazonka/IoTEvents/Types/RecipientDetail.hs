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
-- Module      : Amazonka.IoTEvents.Types.RecipientDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.RecipientDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.SSOIdentity
import qualified Amazonka.Prelude as Prelude

-- | The information that identifies the recipient.
--
-- /See:/ 'newRecipientDetail' smart constructor.
data RecipientDetail = RecipientDetail'
  { -- | The AWS Single Sign-On (AWS SSO) authentication information.
    ssoIdentity :: Prelude.Maybe SSOIdentity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecipientDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ssoIdentity', 'recipientDetail_ssoIdentity' - The AWS Single Sign-On (AWS SSO) authentication information.
newRecipientDetail ::
  RecipientDetail
newRecipientDetail =
  RecipientDetail' {ssoIdentity = Prelude.Nothing}

-- | The AWS Single Sign-On (AWS SSO) authentication information.
recipientDetail_ssoIdentity :: Lens.Lens' RecipientDetail (Prelude.Maybe SSOIdentity)
recipientDetail_ssoIdentity = Lens.lens (\RecipientDetail' {ssoIdentity} -> ssoIdentity) (\s@RecipientDetail' {} a -> s {ssoIdentity = a} :: RecipientDetail)

instance Data.FromJSON RecipientDetail where
  parseJSON =
    Data.withObject
      "RecipientDetail"
      ( \x ->
          RecipientDetail'
            Prelude.<$> (x Data..:? "ssoIdentity")
      )

instance Prelude.Hashable RecipientDetail where
  hashWithSalt _salt RecipientDetail' {..} =
    _salt `Prelude.hashWithSalt` ssoIdentity

instance Prelude.NFData RecipientDetail where
  rnf RecipientDetail' {..} = Prelude.rnf ssoIdentity

instance Data.ToJSON RecipientDetail where
  toJSON RecipientDetail' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ssoIdentity" Data..=) Prelude.<$> ssoIdentity]
      )
