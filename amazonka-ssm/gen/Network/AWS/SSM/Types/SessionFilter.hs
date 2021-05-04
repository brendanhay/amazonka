{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SessionFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.SessionFilterKey

-- | Describes a filter for Session Manager information.
--
-- /See:/ 'newSessionFilter' smart constructor.
data SessionFilter = SessionFilter'
  { -- | The name of the filter.
    key :: SessionFilterKey,
    -- | The filter value. Valid values for each filter key are as follows:
    --
    -- -   InvokedAfter: Specify a timestamp to limit your results. For
    --     example, specify 2018-08-29T00:00:00Z to see sessions that started
    --     August 29, 2018, and later.
    --
    -- -   InvokedBefore: Specify a timestamp to limit your results. For
    --     example, specify 2018-08-29T00:00:00Z to see sessions that started
    --     before August 29, 2018.
    --
    -- -   Target: Specify an instance to which session connections have been
    --     made.
    --
    -- -   Owner: Specify an AWS user account to see a list of sessions started
    --     by that user.
    --
    -- -   Status: Specify a valid session status to see a list of all sessions
    --     with that status. Status values you can specify include:
    --
    --     -   Connected
    --
    --     -   Connecting
    --
    --     -   Disconnected
    --
    --     -   Terminated
    --
    --     -   Terminating
    --
    --     -   Failed
    --
    -- -   SessionId: Specify a session ID to return details about the session.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SessionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'sessionFilter_key' - The name of the filter.
--
-- 'value', 'sessionFilter_value' - The filter value. Valid values for each filter key are as follows:
--
-- -   InvokedAfter: Specify a timestamp to limit your results. For
--     example, specify 2018-08-29T00:00:00Z to see sessions that started
--     August 29, 2018, and later.
--
-- -   InvokedBefore: Specify a timestamp to limit your results. For
--     example, specify 2018-08-29T00:00:00Z to see sessions that started
--     before August 29, 2018.
--
-- -   Target: Specify an instance to which session connections have been
--     made.
--
-- -   Owner: Specify an AWS user account to see a list of sessions started
--     by that user.
--
-- -   Status: Specify a valid session status to see a list of all sessions
--     with that status. Status values you can specify include:
--
--     -   Connected
--
--     -   Connecting
--
--     -   Disconnected
--
--     -   Terminated
--
--     -   Terminating
--
--     -   Failed
--
-- -   SessionId: Specify a session ID to return details about the session.
newSessionFilter ::
  -- | 'key'
  SessionFilterKey ->
  -- | 'value'
  Prelude.Text ->
  SessionFilter
newSessionFilter pKey_ pValue_ =
  SessionFilter' {key = pKey_, value = pValue_}

-- | The name of the filter.
sessionFilter_key :: Lens.Lens' SessionFilter SessionFilterKey
sessionFilter_key = Lens.lens (\SessionFilter' {key} -> key) (\s@SessionFilter' {} a -> s {key = a} :: SessionFilter)

-- | The filter value. Valid values for each filter key are as follows:
--
-- -   InvokedAfter: Specify a timestamp to limit your results. For
--     example, specify 2018-08-29T00:00:00Z to see sessions that started
--     August 29, 2018, and later.
--
-- -   InvokedBefore: Specify a timestamp to limit your results. For
--     example, specify 2018-08-29T00:00:00Z to see sessions that started
--     before August 29, 2018.
--
-- -   Target: Specify an instance to which session connections have been
--     made.
--
-- -   Owner: Specify an AWS user account to see a list of sessions started
--     by that user.
--
-- -   Status: Specify a valid session status to see a list of all sessions
--     with that status. Status values you can specify include:
--
--     -   Connected
--
--     -   Connecting
--
--     -   Disconnected
--
--     -   Terminated
--
--     -   Terminating
--
--     -   Failed
--
-- -   SessionId: Specify a session ID to return details about the session.
sessionFilter_value :: Lens.Lens' SessionFilter Prelude.Text
sessionFilter_value = Lens.lens (\SessionFilter' {value} -> value) (\s@SessionFilter' {} a -> s {value = a} :: SessionFilter)

instance Prelude.Hashable SessionFilter

instance Prelude.NFData SessionFilter

instance Prelude.ToJSON SessionFilter where
  toJSON SessionFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Prelude..= key),
            Prelude.Just ("value" Prelude..= value)
          ]
      )
