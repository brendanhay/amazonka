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
-- Module      : Network.AWS.XRay.Types.RootCauseException
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.RootCauseException where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The exception associated with a root cause.
--
-- /See:/ 'newRootCauseException' smart constructor.
data RootCauseException = RootCauseException'
  { -- | The message of the exception.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the exception.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RootCauseException' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'rootCauseException_message' - The message of the exception.
--
-- 'name', 'rootCauseException_name' - The name of the exception.
newRootCauseException ::
  RootCauseException
newRootCauseException =
  RootCauseException'
    { message = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The message of the exception.
rootCauseException_message :: Lens.Lens' RootCauseException (Prelude.Maybe Prelude.Text)
rootCauseException_message = Lens.lens (\RootCauseException' {message} -> message) (\s@RootCauseException' {} a -> s {message = a} :: RootCauseException)

-- | The name of the exception.
rootCauseException_name :: Lens.Lens' RootCauseException (Prelude.Maybe Prelude.Text)
rootCauseException_name = Lens.lens (\RootCauseException' {name} -> name) (\s@RootCauseException' {} a -> s {name = a} :: RootCauseException)

instance Prelude.FromJSON RootCauseException where
  parseJSON =
    Prelude.withObject
      "RootCauseException"
      ( \x ->
          RootCauseException'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable RootCauseException

instance Prelude.NFData RootCauseException
