{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
  ( CognitoOptionsStatus (..),

    -- * Smart constructor
    mkCognitoOptionsStatus,

    -- * Lenses
    cosStatus,
    cosOptions,
  )
where

import Network.AWS.ElasticSearch.Types.CognitoOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status of the Cognito options for the specified Elasticsearch domain.
--
-- /See:/ 'mkCognitoOptionsStatus' smart constructor.
data CognitoOptionsStatus = CognitoOptionsStatus'
  { -- | Specifies the status of the Cognito options for the specified Elasticsearch domain.
    status :: OptionStatus,
    -- | Specifies the Cognito options for the specified Elasticsearch domain.
    options :: CognitoOptions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CognitoOptionsStatus' with the minimum fields required to make a request.
--
-- * 'status' - Specifies the status of the Cognito options for the specified Elasticsearch domain.
-- * 'options' - Specifies the Cognito options for the specified Elasticsearch domain.
mkCognitoOptionsStatus ::
  -- | 'status'
  OptionStatus ->
  -- | 'options'
  CognitoOptions ->
  CognitoOptionsStatus
mkCognitoOptionsStatus pStatus_ pOptions_ =
  CognitoOptionsStatus' {status = pStatus_, options = pOptions_}

-- | Specifies the status of the Cognito options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cosStatus :: Lens.Lens' CognitoOptionsStatus OptionStatus
cosStatus = Lens.lens (status :: CognitoOptionsStatus -> OptionStatus) (\s a -> s {status = a} :: CognitoOptionsStatus)
{-# DEPRECATED cosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the Cognito options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cosOptions :: Lens.Lens' CognitoOptionsStatus CognitoOptions
cosOptions = Lens.lens (options :: CognitoOptionsStatus -> CognitoOptions) (\s a -> s {options = a} :: CognitoOptionsStatus)
{-# DEPRECATED cosOptions "Use generic-lens or generic-optics with 'options' instead." #-}

instance Lude.FromJSON CognitoOptionsStatus where
  parseJSON =
    Lude.withObject
      "CognitoOptionsStatus"
      ( \x ->
          CognitoOptionsStatus'
            Lude.<$> (x Lude..: "Status") Lude.<*> (x Lude..: "Options")
      )
