{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppType
  ( AppType
      ( ..,
        AppType_JupyterServer,
        AppType_KernelGateway,
        AppType_TensorBoard
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AppType = AppType' {fromAppType :: Core.Text}
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AppType_JupyterServer :: AppType
pattern AppType_JupyterServer = AppType' "JupyterServer"

pattern AppType_KernelGateway :: AppType
pattern AppType_KernelGateway = AppType' "KernelGateway"

pattern AppType_TensorBoard :: AppType
pattern AppType_TensorBoard = AppType' "TensorBoard"

{-# COMPLETE
  AppType_JupyterServer,
  AppType_KernelGateway,
  AppType_TensorBoard,
  AppType'
  #-}
