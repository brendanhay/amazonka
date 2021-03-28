{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.RoomSkillParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.RoomSkillParameter
  ( RoomSkillParameter (..)
  -- * Smart constructor
  , mkRoomSkillParameter
  -- * Lenses
  , rspParameterKey
  , rspParameterValue
  ) where

import qualified Network.AWS.AlexaBusiness.Types.ParameterKey as Types
import qualified Network.AWS.AlexaBusiness.Types.RoomSkillParameterValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A skill parameter associated with a room.
--
-- /See:/ 'mkRoomSkillParameter' smart constructor.
data RoomSkillParameter = RoomSkillParameter'
  { parameterKey :: Types.ParameterKey
    -- ^ The parameter key of a room skill parameter. ParameterKey is an enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
  , parameterValue :: Types.RoomSkillParameterValue
    -- ^ The parameter value of a room skill parameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RoomSkillParameter' value with any optional fields omitted.
mkRoomSkillParameter
    :: Types.ParameterKey -- ^ 'parameterKey'
    -> Types.RoomSkillParameterValue -- ^ 'parameterValue'
    -> RoomSkillParameter
mkRoomSkillParameter parameterKey parameterValue
  = RoomSkillParameter'{parameterKey, parameterValue}

-- | The parameter key of a room skill parameter. ParameterKey is an enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rspParameterKey :: Lens.Lens' RoomSkillParameter Types.ParameterKey
rspParameterKey = Lens.field @"parameterKey"
{-# INLINEABLE rspParameterKey #-}
{-# DEPRECATED parameterKey "Use generic-lens or generic-optics with 'parameterKey' instead"  #-}

-- | The parameter value of a room skill parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rspParameterValue :: Lens.Lens' RoomSkillParameter Types.RoomSkillParameterValue
rspParameterValue = Lens.field @"parameterValue"
{-# INLINEABLE rspParameterValue #-}
{-# DEPRECATED parameterValue "Use generic-lens or generic-optics with 'parameterValue' instead"  #-}

instance Core.FromJSON RoomSkillParameter where
        toJSON RoomSkillParameter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ParameterKey" Core..= parameterKey),
                  Core.Just ("ParameterValue" Core..= parameterValue)])

instance Core.FromJSON RoomSkillParameter where
        parseJSON
          = Core.withObject "RoomSkillParameter" Core.$
              \ x ->
                RoomSkillParameter' Core.<$>
                  (x Core..: "ParameterKey") Core.<*> x Core..: "ParameterValue"
