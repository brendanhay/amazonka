-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SessionFilter
  ( SessionFilter (..),

    -- * Smart constructor
    mkSessionFilter,

    -- * Lenses
    sfKey,
    sfValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.SessionFilterKey

-- | Describes a filter for Session Manager information.
--
-- /See:/ 'mkSessionFilter' smart constructor.
data SessionFilter = SessionFilter'
  { key :: SessionFilterKey,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SessionFilter' with the minimum fields required to make a request.
--
-- * 'key' - The name of the filter.
-- * 'value' - The filter value. Valid values for each filter key are as follows:
--
--
--     * InvokedAfter: Specify a timestamp to limit your results. For example, specify 2018-08-29T00:00:00Z to see sessions that started August 29, 2018, and later.
--
--
--     * InvokedBefore: Specify a timestamp to limit your results. For example, specify 2018-08-29T00:00:00Z to see sessions that started before August 29, 2018.
--
--
--     * Target: Specify an instance to which session connections have been made.
--
--
--     * Owner: Specify an AWS user account to see a list of sessions started by that user.
--
--
--     * Status: Specify a valid session status to see a list of all sessions with that status. Status values you can specify include:
--
--     * Connected
--
--
--     * Connecting
--
--
--     * Disconnected
--
--
--     * Terminated
--
--
--     * Terminating
--
--
--     * Failed
--
--
--
--
--     * SessionId: Specify a session ID to return details about the session.
mkSessionFilter ::
  -- | 'key'
  SessionFilterKey ->
  -- | 'value'
  Lude.Text ->
  SessionFilter
mkSessionFilter pKey_ pValue_ =
  SessionFilter' {key = pKey_, value = pValue_}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfKey :: Lens.Lens' SessionFilter SessionFilterKey
sfKey = Lens.lens (key :: SessionFilter -> SessionFilterKey) (\s a -> s {key = a} :: SessionFilter)
{-# DEPRECATED sfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The filter value. Valid values for each filter key are as follows:
--
--
--     * InvokedAfter: Specify a timestamp to limit your results. For example, specify 2018-08-29T00:00:00Z to see sessions that started August 29, 2018, and later.
--
--
--     * InvokedBefore: Specify a timestamp to limit your results. For example, specify 2018-08-29T00:00:00Z to see sessions that started before August 29, 2018.
--
--
--     * Target: Specify an instance to which session connections have been made.
--
--
--     * Owner: Specify an AWS user account to see a list of sessions started by that user.
--
--
--     * Status: Specify a valid session status to see a list of all sessions with that status. Status values you can specify include:
--
--     * Connected
--
--
--     * Connecting
--
--
--     * Disconnected
--
--
--     * Terminated
--
--
--     * Terminating
--
--
--     * Failed
--
--
--
--
--     * SessionId: Specify a session ID to return details about the session.
--
--
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfValue :: Lens.Lens' SessionFilter Lude.Text
sfValue = Lens.lens (value :: SessionFilter -> Lude.Text) (\s a -> s {value = a} :: SessionFilter)
{-# DEPRECATED sfValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToJSON SessionFilter where
  toJSON SessionFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("key" Lude..= key), Lude.Just ("value" Lude..= value)]
      )
