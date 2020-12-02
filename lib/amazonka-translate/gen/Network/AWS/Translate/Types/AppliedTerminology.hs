{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.AppliedTerminology
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.AppliedTerminology where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Translate.Types.Term

-- | The custom terminology applied to the input text by Amazon Translate for the translated text response. This is optional in the response and will only be present if you specified terminology input in the request. Currently, only one terminology can be applied per TranslateText request.
--
--
--
-- /See:/ 'appliedTerminology' smart constructor.
data AppliedTerminology = AppliedTerminology'
  { _atTerms ::
      !(Maybe [Term]),
    _atName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AppliedTerminology' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atTerms' - The specific terms of the custom terminology applied to the input text by Amazon Translate for the translated text response. A maximum of 250 terms will be returned, and the specific terms applied will be the first 250 terms in the source text.
--
-- * 'atName' - The name of the custom terminology applied to the input text by Amazon Translate for the translated text response.
appliedTerminology ::
  AppliedTerminology
appliedTerminology =
  AppliedTerminology' {_atTerms = Nothing, _atName = Nothing}

-- | The specific terms of the custom terminology applied to the input text by Amazon Translate for the translated text response. A maximum of 250 terms will be returned, and the specific terms applied will be the first 250 terms in the source text.
atTerms :: Lens' AppliedTerminology [Term]
atTerms = lens _atTerms (\s a -> s {_atTerms = a}) . _Default . _Coerce

-- | The name of the custom terminology applied to the input text by Amazon Translate for the translated text response.
atName :: Lens' AppliedTerminology (Maybe Text)
atName = lens _atName (\s a -> s {_atName = a})

instance FromJSON AppliedTerminology where
  parseJSON =
    withObject
      "AppliedTerminology"
      ( \x ->
          AppliedTerminology'
            <$> (x .:? "Terms" .!= mempty) <*> (x .:? "Name")
      )

instance Hashable AppliedTerminology

instance NFData AppliedTerminology
