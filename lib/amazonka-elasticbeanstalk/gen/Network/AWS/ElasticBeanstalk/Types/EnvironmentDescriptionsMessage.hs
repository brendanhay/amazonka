{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage
  ( EnvironmentDescriptionsMessage (..),

    -- * Smart constructor
    mkEnvironmentDescriptionsMessage,

    -- * Lenses
    edmNextToken,
    edmEnvironments,
  )
where

import Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Result message containing a list of environment descriptions.
--
-- /See:/ 'mkEnvironmentDescriptionsMessage' smart constructor.
data EnvironmentDescriptionsMessage = EnvironmentDescriptionsMessage'
  { nextToken ::
      Lude.Maybe Lude.Text,
    environments ::
      Lude.Maybe
        [EnvironmentDescription]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentDescriptionsMessage' with the minimum fields required to make a request.
--
-- * 'environments' - Returns an 'EnvironmentDescription' list.
-- * 'nextToken' - In a paginated request, the token that you can pass in a subsequent request to get the next response page.
mkEnvironmentDescriptionsMessage ::
  EnvironmentDescriptionsMessage
mkEnvironmentDescriptionsMessage =
  EnvironmentDescriptionsMessage'
    { nextToken = Lude.Nothing,
      environments = Lude.Nothing
    }

-- | In a paginated request, the token that you can pass in a subsequent request to get the next response page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edmNextToken :: Lens.Lens' EnvironmentDescriptionsMessage (Lude.Maybe Lude.Text)
edmNextToken = Lens.lens (nextToken :: EnvironmentDescriptionsMessage -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: EnvironmentDescriptionsMessage)
{-# DEPRECATED edmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns an 'EnvironmentDescription' list.
--
-- /Note:/ Consider using 'environments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edmEnvironments :: Lens.Lens' EnvironmentDescriptionsMessage (Lude.Maybe [EnvironmentDescription])
edmEnvironments = Lens.lens (environments :: EnvironmentDescriptionsMessage -> Lude.Maybe [EnvironmentDescription]) (\s a -> s {environments = a} :: EnvironmentDescriptionsMessage)
{-# DEPRECATED edmEnvironments "Use generic-lens or generic-optics with 'environments' instead." #-}

instance Lude.FromXML EnvironmentDescriptionsMessage where
  parseXML x =
    EnvironmentDescriptionsMessage'
      Lude.<$> (x Lude..@? "NextToken")
      Lude.<*> ( x Lude..@? "Environments" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
