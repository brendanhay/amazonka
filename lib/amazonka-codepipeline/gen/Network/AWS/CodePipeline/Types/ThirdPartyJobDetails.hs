{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ThirdPartyJobDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ThirdPartyJobDetails
  ( ThirdPartyJobDetails (..),

    -- * Smart constructor
    mkThirdPartyJobDetails,

    -- * Lenses
    tpjdData,
    tpjdId,
    tpjdNonce,
  )
where

import Network.AWS.CodePipeline.Types.ThirdPartyJobData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of a job sent in response to a @GetThirdPartyJobDetails@ request.
--
-- /See:/ 'mkThirdPartyJobDetails' smart constructor.
data ThirdPartyJobDetails = ThirdPartyJobDetails'
  { data' ::
      Lude.Maybe ThirdPartyJobData,
    id :: Lude.Maybe Lude.Text,
    nonce :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThirdPartyJobDetails' with the minimum fields required to make a request.
--
-- * 'data'' - The data to be returned by the third party job worker.
-- * 'id' - The identifier used to identify the job details in AWS CodePipeline.
-- * 'nonce' - A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Use this number in an 'AcknowledgeThirdPartyJob' request.
mkThirdPartyJobDetails ::
  ThirdPartyJobDetails
mkThirdPartyJobDetails =
  ThirdPartyJobDetails'
    { data' = Lude.Nothing,
      id = Lude.Nothing,
      nonce = Lude.Nothing
    }

-- | The data to be returned by the third party job worker.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpjdData :: Lens.Lens' ThirdPartyJobDetails (Lude.Maybe ThirdPartyJobData)
tpjdData = Lens.lens (data' :: ThirdPartyJobDetails -> Lude.Maybe ThirdPartyJobData) (\s a -> s {data' = a} :: ThirdPartyJobDetails)
{-# DEPRECATED tpjdData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The identifier used to identify the job details in AWS CodePipeline.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpjdId :: Lens.Lens' ThirdPartyJobDetails (Lude.Maybe Lude.Text)
tpjdId = Lens.lens (id :: ThirdPartyJobDetails -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ThirdPartyJobDetails)
{-# DEPRECATED tpjdId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Use this number in an 'AcknowledgeThirdPartyJob' request.
--
-- /Note:/ Consider using 'nonce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpjdNonce :: Lens.Lens' ThirdPartyJobDetails (Lude.Maybe Lude.Text)
tpjdNonce = Lens.lens (nonce :: ThirdPartyJobDetails -> Lude.Maybe Lude.Text) (\s a -> s {nonce = a} :: ThirdPartyJobDetails)
{-# DEPRECATED tpjdNonce "Use generic-lens or generic-optics with 'nonce' instead." #-}

instance Lude.FromJSON ThirdPartyJobDetails where
  parseJSON =
    Lude.withObject
      "ThirdPartyJobDetails"
      ( \x ->
          ThirdPartyJobDetails'
            Lude.<$> (x Lude..:? "data")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "nonce")
      )
