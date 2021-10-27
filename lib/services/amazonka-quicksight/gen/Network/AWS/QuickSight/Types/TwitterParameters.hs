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
-- Module      : Network.AWS.QuickSight.Types.TwitterParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.TwitterParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The parameters for Twitter.
--
-- /See:/ 'newTwitterParameters' smart constructor.
data TwitterParameters = TwitterParameters'
  { -- | Twitter query string.
    query :: Prelude.Text,
    -- | Maximum number of rows to query Twitter.
    maxRows :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TwitterParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'query', 'twitterParameters_query' - Twitter query string.
--
-- 'maxRows', 'twitterParameters_maxRows' - Maximum number of rows to query Twitter.
newTwitterParameters ::
  -- | 'query'
  Prelude.Text ->
  -- | 'maxRows'
  Prelude.Natural ->
  TwitterParameters
newTwitterParameters pQuery_ pMaxRows_ =
  TwitterParameters'
    { query = pQuery_,
      maxRows = pMaxRows_
    }

-- | Twitter query string.
twitterParameters_query :: Lens.Lens' TwitterParameters Prelude.Text
twitterParameters_query = Lens.lens (\TwitterParameters' {query} -> query) (\s@TwitterParameters' {} a -> s {query = a} :: TwitterParameters)

-- | Maximum number of rows to query Twitter.
twitterParameters_maxRows :: Lens.Lens' TwitterParameters Prelude.Natural
twitterParameters_maxRows = Lens.lens (\TwitterParameters' {maxRows} -> maxRows) (\s@TwitterParameters' {} a -> s {maxRows = a} :: TwitterParameters)

instance Core.FromJSON TwitterParameters where
  parseJSON =
    Core.withObject
      "TwitterParameters"
      ( \x ->
          TwitterParameters'
            Prelude.<$> (x Core..: "Query")
            Prelude.<*> (x Core..: "MaxRows")
      )

instance Prelude.Hashable TwitterParameters

instance Prelude.NFData TwitterParameters

instance Core.ToJSON TwitterParameters where
  toJSON TwitterParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Query" Core..= query),
            Prelude.Just ("MaxRows" Core..= maxRows)
          ]
      )
