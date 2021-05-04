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
-- Module      : Network.AWS.LexRuntime.Types.ResponseCard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.ResponseCard where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.ContentType
import Network.AWS.LexRuntime.Types.GenericAttachment
import qualified Network.AWS.Prelude as Prelude

-- | If you configure a response card when creating your bots, Amazon Lex
-- substitutes the session attributes and slot values that are available,
-- and then returns it. The response card can also come from a Lambda
-- function ( @dialogCodeHook@ and @fulfillmentActivity@ on an intent).
--
-- /See:/ 'newResponseCard' smart constructor.
data ResponseCard = ResponseCard'
  { -- | The content type of the response.
    contentType :: Prelude.Maybe ContentType,
    -- | An array of attachment objects representing options.
    genericAttachments :: Prelude.Maybe [GenericAttachment],
    -- | The version of the response card format.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResponseCard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'responseCard_contentType' - The content type of the response.
--
-- 'genericAttachments', 'responseCard_genericAttachments' - An array of attachment objects representing options.
--
-- 'version', 'responseCard_version' - The version of the response card format.
newResponseCard ::
  ResponseCard
newResponseCard =
  ResponseCard'
    { contentType = Prelude.Nothing,
      genericAttachments = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The content type of the response.
responseCard_contentType :: Lens.Lens' ResponseCard (Prelude.Maybe ContentType)
responseCard_contentType = Lens.lens (\ResponseCard' {contentType} -> contentType) (\s@ResponseCard' {} a -> s {contentType = a} :: ResponseCard)

-- | An array of attachment objects representing options.
responseCard_genericAttachments :: Lens.Lens' ResponseCard (Prelude.Maybe [GenericAttachment])
responseCard_genericAttachments = Lens.lens (\ResponseCard' {genericAttachments} -> genericAttachments) (\s@ResponseCard' {} a -> s {genericAttachments = a} :: ResponseCard) Prelude.. Lens.mapping Prelude._Coerce

-- | The version of the response card format.
responseCard_version :: Lens.Lens' ResponseCard (Prelude.Maybe Prelude.Text)
responseCard_version = Lens.lens (\ResponseCard' {version} -> version) (\s@ResponseCard' {} a -> s {version = a} :: ResponseCard)

instance Prelude.FromJSON ResponseCard where
  parseJSON =
    Prelude.withObject
      "ResponseCard"
      ( \x ->
          ResponseCard'
            Prelude.<$> (x Prelude..:? "contentType")
            Prelude.<*> ( x Prelude..:? "genericAttachments"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "version")
      )

instance Prelude.Hashable ResponseCard

instance Prelude.NFData ResponseCard
