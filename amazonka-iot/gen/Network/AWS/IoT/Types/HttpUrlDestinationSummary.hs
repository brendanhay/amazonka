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
-- Module      : Network.AWS.IoT.Types.HttpUrlDestinationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HttpUrlDestinationSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an HTTP URL destination.
--
-- /See:/ 'newHttpUrlDestinationSummary' smart constructor.
data HttpUrlDestinationSummary = HttpUrlDestinationSummary'
  { -- | The URL used to confirm ownership of or access to the HTTP topic rule
    -- destination URL.
    confirmationUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HttpUrlDestinationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confirmationUrl', 'httpUrlDestinationSummary_confirmationUrl' - The URL used to confirm ownership of or access to the HTTP topic rule
-- destination URL.
newHttpUrlDestinationSummary ::
  HttpUrlDestinationSummary
newHttpUrlDestinationSummary =
  HttpUrlDestinationSummary'
    { confirmationUrl =
        Prelude.Nothing
    }

-- | The URL used to confirm ownership of or access to the HTTP topic rule
-- destination URL.
httpUrlDestinationSummary_confirmationUrl :: Lens.Lens' HttpUrlDestinationSummary (Prelude.Maybe Prelude.Text)
httpUrlDestinationSummary_confirmationUrl = Lens.lens (\HttpUrlDestinationSummary' {confirmationUrl} -> confirmationUrl) (\s@HttpUrlDestinationSummary' {} a -> s {confirmationUrl = a} :: HttpUrlDestinationSummary)

instance Prelude.FromJSON HttpUrlDestinationSummary where
  parseJSON =
    Prelude.withObject
      "HttpUrlDestinationSummary"
      ( \x ->
          HttpUrlDestinationSummary'
            Prelude.<$> (x Prelude..:? "confirmationUrl")
      )

instance Prelude.Hashable HttpUrlDestinationSummary

instance Prelude.NFData HttpUrlDestinationSummary
