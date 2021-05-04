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
-- Module      : Network.AWS.IoT.Types.SalesforceAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SalesforceAction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an action to write a message to a Salesforce IoT Cloud Input
-- Stream.
--
-- /See:/ 'newSalesforceAction' smart constructor.
data SalesforceAction = SalesforceAction'
  { -- | The token used to authenticate access to the Salesforce IoT Cloud Input
    -- Stream. The token is available from the Salesforce IoT Cloud platform
    -- after creation of the Input Stream.
    token :: Prelude.Text,
    -- | The URL exposed by the Salesforce IoT Cloud Input Stream. The URL is
    -- available from the Salesforce IoT Cloud platform after creation of the
    -- Input Stream.
    url :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SalesforceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'token', 'salesforceAction_token' - The token used to authenticate access to the Salesforce IoT Cloud Input
-- Stream. The token is available from the Salesforce IoT Cloud platform
-- after creation of the Input Stream.
--
-- 'url', 'salesforceAction_url' - The URL exposed by the Salesforce IoT Cloud Input Stream. The URL is
-- available from the Salesforce IoT Cloud platform after creation of the
-- Input Stream.
newSalesforceAction ::
  -- | 'token'
  Prelude.Text ->
  -- | 'url'
  Prelude.Text ->
  SalesforceAction
newSalesforceAction pToken_ pUrl_ =
  SalesforceAction' {token = pToken_, url = pUrl_}

-- | The token used to authenticate access to the Salesforce IoT Cloud Input
-- Stream. The token is available from the Salesforce IoT Cloud platform
-- after creation of the Input Stream.
salesforceAction_token :: Lens.Lens' SalesforceAction Prelude.Text
salesforceAction_token = Lens.lens (\SalesforceAction' {token} -> token) (\s@SalesforceAction' {} a -> s {token = a} :: SalesforceAction)

-- | The URL exposed by the Salesforce IoT Cloud Input Stream. The URL is
-- available from the Salesforce IoT Cloud platform after creation of the
-- Input Stream.
salesforceAction_url :: Lens.Lens' SalesforceAction Prelude.Text
salesforceAction_url = Lens.lens (\SalesforceAction' {url} -> url) (\s@SalesforceAction' {} a -> s {url = a} :: SalesforceAction)

instance Prelude.FromJSON SalesforceAction where
  parseJSON =
    Prelude.withObject
      "SalesforceAction"
      ( \x ->
          SalesforceAction'
            Prelude.<$> (x Prelude..: "token")
            Prelude.<*> (x Prelude..: "url")
      )

instance Prelude.Hashable SalesforceAction

instance Prelude.NFData SalesforceAction

instance Prelude.ToJSON SalesforceAction where
  toJSON SalesforceAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("token" Prelude..= token),
            Prelude.Just ("url" Prelude..= url)
          ]
      )
