-- |
-- Module      : Amazonka.Core
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Types and values referenced by generated code.
module Amazonka.Core
  ( module Amazonka.Types,
    module Amazonka.Endpoint,
    module Amazonka.Data,
    module Amazonka.Pager,
    module Amazonka.Waiter,
    module Amazonka.Error,
    (.!@),
    may,
  )
where

-- Export Amazonka.Data submodules piecemeal and avoid exporting AWS
-- encoding/decoding modules, so we don't leak too much to library
-- clients. Service bindings import Amazonka.Data directly.
import Amazonka.Data.Base64 as Amazonka.Data
import Amazonka.Data.Body as Amazonka.Data
import Amazonka.Data.Log as Amazonka.Data
import Amazonka.Data.Sensitive as Amazonka.Data
import Amazonka.Data.Time as Amazonka.Data
import Amazonka.Endpoint
import Amazonka.Error
import Amazonka.Pager
import Amazonka.Prelude
import Amazonka.Types
import Amazonka.Waiter

-- Legacy code generation operators

infixl 7 .!@

(.!@) :: Functor f => f (Maybe a) -> a -> f a
f .!@ x = fromMaybe x <$> f

may :: Applicative f => ([a] -> f b) -> [a] -> f (Maybe b)
may _ [] = pure Nothing
may f xs = Just <$> f xs
