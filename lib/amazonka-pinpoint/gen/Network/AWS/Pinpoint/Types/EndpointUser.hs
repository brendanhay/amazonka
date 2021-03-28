{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EndpointUser
  ( EndpointUser (..)
  -- * Smart constructor
  , mkEndpointUser
  -- * Lenses
  , euUserAttributes
  , euUserId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies data for one or more attributes that describe the user who's associated with an endpoint.
--
-- /See:/ 'mkEndpointUser' smart constructor.
data EndpointUser = EndpointUser'
  { userAttributes :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ One or more custom attributes that describe the user by associating a name with an array of values. For example, the value of an attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
  , userId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointUser' value with any optional fields omitted.
mkEndpointUser
    :: EndpointUser
mkEndpointUser
  = EndpointUser'{userAttributes = Core.Nothing,
                  userId = Core.Nothing}

-- | One or more custom attributes that describe the user by associating a name with an array of values. For example, the value of an attribute named Interests might be: ["Science", "Music", "Travel"]. You can use these attributes as filter criteria when you create segments. Attribute names are case sensitive.
--
-- An attribute name can contain up to 50 characters. An attribute value can contain up to 100 characters. When you define the name of a custom attribute, avoid using the following characters: number sign (#), colon (:), question mark (?), backslash (\), and slash (/). The Amazon Pinpoint console can't display attribute names that contain these characters. This restriction doesn't apply to attribute values.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
euUserAttributes :: Lens.Lens' EndpointUser (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
euUserAttributes = Lens.field @"userAttributes"
{-# INLINEABLE euUserAttributes #-}
{-# DEPRECATED userAttributes "Use generic-lens or generic-optics with 'userAttributes' instead"  #-}

-- | The unique identifier for the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
euUserId :: Lens.Lens' EndpointUser (Core.Maybe Core.Text)
euUserId = Lens.field @"userId"
{-# INLINEABLE euUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

instance Core.FromJSON EndpointUser where
        toJSON EndpointUser{..}
          = Core.object
              (Core.catMaybes
                 [("UserAttributes" Core..=) Core.<$> userAttributes,
                  ("UserId" Core..=) Core.<$> userId])

instance Core.FromJSON EndpointUser where
        parseJSON
          = Core.withObject "EndpointUser" Core.$
              \ x ->
                EndpointUser' Core.<$>
                  (x Core..:? "UserAttributes") Core.<*> x Core..:? "UserId"
