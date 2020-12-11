-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.MasterUserOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.MasterUserOptions
  ( MasterUserOptions (..),

    -- * Smart constructor
    mkMasterUserOptions,

    -- * Lenses
    muoMasterUserPassword,
    muoMasterUserName,
    muoMasterUserARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Credentials for the master user: username and password, ARN, or both.
--
-- /See:/ 'mkMasterUserOptions' smart constructor.
data MasterUserOptions = MasterUserOptions'
  { masterUserPassword ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    masterUserName :: Lude.Maybe (Lude.Sensitive Lude.Text),
    masterUserARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MasterUserOptions' with the minimum fields required to make a request.
--
-- * 'masterUserARN' - ARN for the master user (if IAM is enabled).
-- * 'masterUserName' - The master user's username, which is stored in the Amazon Elasticsearch Service domain's internal database.
-- * 'masterUserPassword' - The master user's password, which is stored in the Amazon Elasticsearch Service domain's internal database.
mkMasterUserOptions ::
  MasterUserOptions
mkMasterUserOptions =
  MasterUserOptions'
    { masterUserPassword = Lude.Nothing,
      masterUserName = Lude.Nothing,
      masterUserARN = Lude.Nothing
    }

-- | The master user's password, which is stored in the Amazon Elasticsearch Service domain's internal database.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muoMasterUserPassword :: Lens.Lens' MasterUserOptions (Lude.Maybe (Lude.Sensitive Lude.Text))
muoMasterUserPassword = Lens.lens (masterUserPassword :: MasterUserOptions -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {masterUserPassword = a} :: MasterUserOptions)
{-# DEPRECATED muoMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | The master user's username, which is stored in the Amazon Elasticsearch Service domain's internal database.
--
-- /Note:/ Consider using 'masterUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muoMasterUserName :: Lens.Lens' MasterUserOptions (Lude.Maybe (Lude.Sensitive Lude.Text))
muoMasterUserName = Lens.lens (masterUserName :: MasterUserOptions -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {masterUserName = a} :: MasterUserOptions)
{-# DEPRECATED muoMasterUserName "Use generic-lens or generic-optics with 'masterUserName' instead." #-}

-- | ARN for the master user (if IAM is enabled).
--
-- /Note:/ Consider using 'masterUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muoMasterUserARN :: Lens.Lens' MasterUserOptions (Lude.Maybe Lude.Text)
muoMasterUserARN = Lens.lens (masterUserARN :: MasterUserOptions -> Lude.Maybe Lude.Text) (\s a -> s {masterUserARN = a} :: MasterUserOptions)
{-# DEPRECATED muoMasterUserARN "Use generic-lens or generic-optics with 'masterUserARN' instead." #-}

instance Lude.ToJSON MasterUserOptions where
  toJSON MasterUserOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MasterUserPassword" Lude..=) Lude.<$> masterUserPassword,
            ("MasterUserName" Lude..=) Lude.<$> masterUserName,
            ("MasterUserARN" Lude..=) Lude.<$> masterUserARN
          ]
      )
