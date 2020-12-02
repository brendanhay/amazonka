{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteUtterances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes stored utterances.
--
--
-- Amazon Lex stores the utterances that users send to your bot. Utterances are stored for 15 days for use with the 'GetUtterancesView' operation, and then stored indefinitely for use in improving the ability of your bot to respond to user input.
--
-- Use the @DeleteStoredUtterances@ operation to manually delete stored utterances for a specific user.
--
-- This operation requires permissions for the @lex:DeleteUtterances@ action.
--
module Network.AWS.LexModels.DeleteUtterances
    (
    -- * Creating a Request
      deleteUtterances
    , DeleteUtterances
    -- * Request Lenses
    , duBotName
    , duUserId

    -- * Destructuring the Response
    , deleteUtterancesResponse
    , DeleteUtterancesResponse
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUtterances' smart constructor.
data DeleteUtterances = DeleteUtterances'
  { _duBotName :: !Text
  , _duUserId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUtterances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duBotName' - The name of the bot that stored the utterances.
--
-- * 'duUserId' - The unique identifier for the user that made the utterances. This is the user ID that was sent in the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> operation request that contained the utterance.
deleteUtterances
    :: Text -- ^ 'duBotName'
    -> Text -- ^ 'duUserId'
    -> DeleteUtterances
deleteUtterances pBotName_ pUserId_ =
  DeleteUtterances' {_duBotName = pBotName_, _duUserId = pUserId_}


-- | The name of the bot that stored the utterances.
duBotName :: Lens' DeleteUtterances Text
duBotName = lens _duBotName (\ s a -> s{_duBotName = a})

-- | The unique identifier for the user that made the utterances. This is the user ID that was sent in the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> operation request that contained the utterance.
duUserId :: Lens' DeleteUtterances Text
duUserId = lens _duUserId (\ s a -> s{_duUserId = a})

instance AWSRequest DeleteUtterances where
        type Rs DeleteUtterances = DeleteUtterancesResponse
        request = delete lexModels
        response = receiveNull DeleteUtterancesResponse'

instance Hashable DeleteUtterances where

instance NFData DeleteUtterances where

instance ToHeaders DeleteUtterances where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteUtterances where
        toPath DeleteUtterances'{..}
          = mconcat
              ["/bots/", toBS _duBotName, "/utterances/",
               toBS _duUserId]

instance ToQuery DeleteUtterances where
        toQuery = const mempty

-- | /See:/ 'deleteUtterancesResponse' smart constructor.
data DeleteUtterancesResponse =
  DeleteUtterancesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUtterancesResponse' with the minimum fields required to make a request.
--
deleteUtterancesResponse
    :: DeleteUtterancesResponse
deleteUtterancesResponse = DeleteUtterancesResponse'


instance NFData DeleteUtterancesResponse where
