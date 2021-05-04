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
-- Module      : Network.AWS.Lightsail.Types.QueryStringObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.QueryStringObject where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the query string parameters that an Amazon Lightsail content
-- delivery network (CDN) distribution to bases caching on.
--
-- For the query strings that you specify, your distribution caches
-- separate versions of the specified content based on the query string
-- values in viewer requests.
--
-- /See:/ 'newQueryStringObject' smart constructor.
data QueryStringObject = QueryStringObject'
  { -- | The specific query strings that the distribution forwards to the origin.
    --
    -- Your distribution will cache content based on the specified query
    -- strings.
    --
    -- If the @option@ parameter is true, then your distribution forwards all
    -- query strings, regardless of what you specify using the
    -- @queryStringsAllowList@ parameter.
    queryStringsAllowList :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether the distribution forwards and caches based on query
    -- strings.
    option :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'QueryStringObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryStringsAllowList', 'queryStringObject_queryStringsAllowList' - The specific query strings that the distribution forwards to the origin.
--
-- Your distribution will cache content based on the specified query
-- strings.
--
-- If the @option@ parameter is true, then your distribution forwards all
-- query strings, regardless of what you specify using the
-- @queryStringsAllowList@ parameter.
--
-- 'option', 'queryStringObject_option' - Indicates whether the distribution forwards and caches based on query
-- strings.
newQueryStringObject ::
  QueryStringObject
newQueryStringObject =
  QueryStringObject'
    { queryStringsAllowList =
        Prelude.Nothing,
      option = Prelude.Nothing
    }

-- | The specific query strings that the distribution forwards to the origin.
--
-- Your distribution will cache content based on the specified query
-- strings.
--
-- If the @option@ parameter is true, then your distribution forwards all
-- query strings, regardless of what you specify using the
-- @queryStringsAllowList@ parameter.
queryStringObject_queryStringsAllowList :: Lens.Lens' QueryStringObject (Prelude.Maybe [Prelude.Text])
queryStringObject_queryStringsAllowList = Lens.lens (\QueryStringObject' {queryStringsAllowList} -> queryStringsAllowList) (\s@QueryStringObject' {} a -> s {queryStringsAllowList = a} :: QueryStringObject) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether the distribution forwards and caches based on query
-- strings.
queryStringObject_option :: Lens.Lens' QueryStringObject (Prelude.Maybe Prelude.Bool)
queryStringObject_option = Lens.lens (\QueryStringObject' {option} -> option) (\s@QueryStringObject' {} a -> s {option = a} :: QueryStringObject)

instance Prelude.FromJSON QueryStringObject where
  parseJSON =
    Prelude.withObject
      "QueryStringObject"
      ( \x ->
          QueryStringObject'
            Prelude.<$> ( x Prelude..:? "queryStringsAllowList"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "option")
      )

instance Prelude.Hashable QueryStringObject

instance Prelude.NFData QueryStringObject

instance Prelude.ToJSON QueryStringObject where
  toJSON QueryStringObject' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("queryStringsAllowList" Prelude..=)
              Prelude.<$> queryStringsAllowList,
            ("option" Prelude..=) Prelude.<$> option
          ]
      )
