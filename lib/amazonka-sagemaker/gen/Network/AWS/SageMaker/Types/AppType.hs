{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppType where

import Network.AWS.Prelude

data AppType
  = JupyterServer
  | KernelGateway
  | TensorBoard
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText AppType where
  parser =
    takeLowerText >>= \case
      "jupyterserver" -> pure JupyterServer
      "kernelgateway" -> pure KernelGateway
      "tensorboard" -> pure TensorBoard
      e ->
        fromTextError $
          "Failure parsing AppType from value: '" <> e
            <> "'. Accepted values: jupyterserver, kernelgateway, tensorboard"

instance ToText AppType where
  toText = \case
    JupyterServer -> "JupyterServer"
    KernelGateway -> "KernelGateway"
    TensorBoard -> "TensorBoard"

instance Hashable AppType

instance NFData AppType

instance ToByteString AppType

instance ToQuery AppType

instance ToHeader AppType

instance ToJSON AppType where
  toJSON = toJSONText

instance FromJSON AppType where
  parseJSON = parseJSONText "AppType"
