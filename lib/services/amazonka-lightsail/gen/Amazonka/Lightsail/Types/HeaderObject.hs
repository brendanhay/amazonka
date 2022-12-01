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
-- Module      : Amazonka.Lightsail.Types.HeaderObject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.HeaderObject where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types.ForwardValues
import Amazonka.Lightsail.Types.HeaderEnum
import qualified Amazonka.Prelude as Prelude

-- | Describes the request headers that a Lightsail distribution bases
-- caching on.
--
-- For the headers that you specify, your distribution caches separate
-- versions of the specified content based on the header values in viewer
-- requests. For example, suppose viewer requests for @logo.jpg@ contain a
-- custom @product@ header that has a value of either @acme@ or @apex@, and
-- you configure your distribution to cache your content based on values in
-- the @product@ header. Your distribution forwards the @product@ header to
-- the origin and caches the response from the origin once for each header
-- value.
--
-- /See:/ 'newHeaderObject' smart constructor.
data HeaderObject = HeaderObject'
  { -- | The headers that you want your distribution to forward to your origin
    -- and base caching on.
    --
    -- You can configure your distribution to do one of the following:
    --
    -- -   __@all@__ - Forward all headers to your origin.
    --
    -- -   __@none@__ - Forward only the default headers.
    --
    -- -   __@allow-list@__ - Forward only the headers you specify using the
    --     @headersAllowList@ parameter.
    option :: Prelude.Maybe ForwardValues,
    -- | The specific headers to forward to your distribution\'s origin.
    headersAllowList :: Prelude.Maybe [HeaderEnum]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeaderObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'option', 'headerObject_option' - The headers that you want your distribution to forward to your origin
-- and base caching on.
--
-- You can configure your distribution to do one of the following:
--
-- -   __@all@__ - Forward all headers to your origin.
--
-- -   __@none@__ - Forward only the default headers.
--
-- -   __@allow-list@__ - Forward only the headers you specify using the
--     @headersAllowList@ parameter.
--
-- 'headersAllowList', 'headerObject_headersAllowList' - The specific headers to forward to your distribution\'s origin.
newHeaderObject ::
  HeaderObject
newHeaderObject =
  HeaderObject'
    { option = Prelude.Nothing,
      headersAllowList = Prelude.Nothing
    }

-- | The headers that you want your distribution to forward to your origin
-- and base caching on.
--
-- You can configure your distribution to do one of the following:
--
-- -   __@all@__ - Forward all headers to your origin.
--
-- -   __@none@__ - Forward only the default headers.
--
-- -   __@allow-list@__ - Forward only the headers you specify using the
--     @headersAllowList@ parameter.
headerObject_option :: Lens.Lens' HeaderObject (Prelude.Maybe ForwardValues)
headerObject_option = Lens.lens (\HeaderObject' {option} -> option) (\s@HeaderObject' {} a -> s {option = a} :: HeaderObject)

-- | The specific headers to forward to your distribution\'s origin.
headerObject_headersAllowList :: Lens.Lens' HeaderObject (Prelude.Maybe [HeaderEnum])
headerObject_headersAllowList = Lens.lens (\HeaderObject' {headersAllowList} -> headersAllowList) (\s@HeaderObject' {} a -> s {headersAllowList = a} :: HeaderObject) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON HeaderObject where
  parseJSON =
    Core.withObject
      "HeaderObject"
      ( \x ->
          HeaderObject'
            Prelude.<$> (x Core..:? "option")
            Prelude.<*> ( x Core..:? "headersAllowList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable HeaderObject where
  hashWithSalt _salt HeaderObject' {..} =
    _salt `Prelude.hashWithSalt` option
      `Prelude.hashWithSalt` headersAllowList

instance Prelude.NFData HeaderObject where
  rnf HeaderObject' {..} =
    Prelude.rnf option
      `Prelude.seq` Prelude.rnf headersAllowList

instance Core.ToJSON HeaderObject where
  toJSON HeaderObject' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("option" Core..=) Prelude.<$> option,
            ("headersAllowList" Core..=)
              Prelude.<$> headersAllowList
          ]
      )
