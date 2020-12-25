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
    edmEnvironments,
    edmNextToken,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Token as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Result message containing a list of environment descriptions.
--
-- /See:/ 'mkEnvironmentDescriptionsMessage' smart constructor.
data EnvironmentDescriptionsMessage = EnvironmentDescriptionsMessage'
  { -- | Returns an 'EnvironmentDescription' list.
    environments :: Core.Maybe [Types.EnvironmentDescription],
    -- | In a paginated request, the token that you can pass in a subsequent request to get the next response page.
    nextToken :: Core.Maybe Types.Token
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EnvironmentDescriptionsMessage' value with any optional fields omitted.
mkEnvironmentDescriptionsMessage ::
  EnvironmentDescriptionsMessage
mkEnvironmentDescriptionsMessage =
  EnvironmentDescriptionsMessage'
    { environments = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Returns an 'EnvironmentDescription' list.
--
-- /Note:/ Consider using 'environments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edmEnvironments :: Lens.Lens' EnvironmentDescriptionsMessage (Core.Maybe [Types.EnvironmentDescription])
edmEnvironments = Lens.field @"environments"
{-# DEPRECATED edmEnvironments "Use generic-lens or generic-optics with 'environments' instead." #-}

-- | In a paginated request, the token that you can pass in a subsequent request to get the next response page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edmNextToken :: Lens.Lens' EnvironmentDescriptionsMessage (Core.Maybe Types.Token)
edmNextToken = Lens.field @"nextToken"
{-# DEPRECATED edmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromXML EnvironmentDescriptionsMessage where
  parseXML x =
    EnvironmentDescriptionsMessage'
      Core.<$> (x Core..@? "Environments" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "NextToken")
