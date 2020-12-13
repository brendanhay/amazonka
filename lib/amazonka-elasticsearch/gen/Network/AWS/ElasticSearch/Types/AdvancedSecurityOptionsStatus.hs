{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
  ( AdvancedSecurityOptionsStatus (..),

    -- * Smart constructor
    mkAdvancedSecurityOptionsStatus,

    -- * Lenses
    asosStatus,
    asosOptions,
  )
where

import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the status of advanced security options for the specified Elasticsearch domain.
--
-- /See:/ 'mkAdvancedSecurityOptionsStatus' smart constructor.
data AdvancedSecurityOptionsStatus = AdvancedSecurityOptionsStatus'
  { -- | Status of the advanced security options for the specified Elasticsearch domain.
    status :: OptionStatus,
    -- | Specifies advanced security options for the specified Elasticsearch domain.
    options :: AdvancedSecurityOptions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdvancedSecurityOptionsStatus' with the minimum fields required to make a request.
--
-- * 'status' - Status of the advanced security options for the specified Elasticsearch domain.
-- * 'options' - Specifies advanced security options for the specified Elasticsearch domain.
mkAdvancedSecurityOptionsStatus ::
  -- | 'status'
  OptionStatus ->
  -- | 'options'
  AdvancedSecurityOptions ->
  AdvancedSecurityOptionsStatus
mkAdvancedSecurityOptionsStatus pStatus_ pOptions_ =
  AdvancedSecurityOptionsStatus'
    { status = pStatus_,
      options = pOptions_
    }

-- | Status of the advanced security options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asosStatus :: Lens.Lens' AdvancedSecurityOptionsStatus OptionStatus
asosStatus = Lens.lens (status :: AdvancedSecurityOptionsStatus -> OptionStatus) (\s a -> s {status = a} :: AdvancedSecurityOptionsStatus)
{-# DEPRECATED asosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies advanced security options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asosOptions :: Lens.Lens' AdvancedSecurityOptionsStatus AdvancedSecurityOptions
asosOptions = Lens.lens (options :: AdvancedSecurityOptionsStatus -> AdvancedSecurityOptions) (\s a -> s {options = a} :: AdvancedSecurityOptionsStatus)
{-# DEPRECATED asosOptions "Use generic-lens or generic-optics with 'options' instead." #-}

instance Lude.FromJSON AdvancedSecurityOptionsStatus where
  parseJSON =
    Lude.withObject
      "AdvancedSecurityOptionsStatus"
      ( \x ->
          AdvancedSecurityOptionsStatus'
            Lude.<$> (x Lude..: "Status") Lude.<*> (x Lude..: "Options")
      )
