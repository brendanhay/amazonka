{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SalesforceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SalesforceAction
  ( SalesforceAction (..),

    -- * Smart constructor
    mkSalesforceAction,

    -- * Lenses
    saUrl,
    saToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action to write a message to a Salesforce IoT Cloud Input Stream.
--
-- /See:/ 'mkSalesforceAction' smart constructor.
data SalesforceAction = SalesforceAction'
  { -- | The URL exposed by the Salesforce IoT Cloud Input Stream. The URL is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
    url :: Lude.Text,
    -- | The token used to authenticate access to the Salesforce IoT Cloud Input Stream. The token is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
    token :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SalesforceAction' with the minimum fields required to make a request.
--
-- * 'url' - The URL exposed by the Salesforce IoT Cloud Input Stream. The URL is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
-- * 'token' - The token used to authenticate access to the Salesforce IoT Cloud Input Stream. The token is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
mkSalesforceAction ::
  -- | 'url'
  Lude.Text ->
  -- | 'token'
  Lude.Text ->
  SalesforceAction
mkSalesforceAction pUrl_ pToken_ =
  SalesforceAction' {url = pUrl_, token = pToken_}

-- | The URL exposed by the Salesforce IoT Cloud Input Stream. The URL is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saUrl :: Lens.Lens' SalesforceAction Lude.Text
saUrl = Lens.lens (url :: SalesforceAction -> Lude.Text) (\s a -> s {url = a} :: SalesforceAction)
{-# DEPRECATED saUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The token used to authenticate access to the Salesforce IoT Cloud Input Stream. The token is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saToken :: Lens.Lens' SalesforceAction Lude.Text
saToken = Lens.lens (token :: SalesforceAction -> Lude.Text) (\s a -> s {token = a} :: SalesforceAction)
{-# DEPRECATED saToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Lude.FromJSON SalesforceAction where
  parseJSON =
    Lude.withObject
      "SalesforceAction"
      ( \x ->
          SalesforceAction'
            Lude.<$> (x Lude..: "url") Lude.<*> (x Lude..: "token")
      )

instance Lude.ToJSON SalesforceAction where
  toJSON SalesforceAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("url" Lude..= url), Lude.Just ("token" Lude..= token)]
      )
