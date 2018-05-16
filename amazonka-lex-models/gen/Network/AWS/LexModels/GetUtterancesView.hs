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
-- Module      : Network.AWS.LexModels.GetUtterancesView
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the @GetUtterancesView@ operation to get information about the utterances that your users have made to your bot. You can use this list to tune the utterances that your bot responds to.
--
--
-- For example, say that you have created a bot to order flowers. After your users have used your bot for a while, use the @GetUtterancesView@ operation to see the requests that they have made and whether they have been successful. You might find that the utterance "I want flowers" is not being recognized. You could add this utterance to the @OrderFlowers@ intent so that your bot recognizes that utterance.
--
-- After you publish a new version of a bot, you can get information about the old version and the new so that you can compare the performance across the two versions.
--
-- This operation requires permissions for the @lex:GetUtterancesView@ action.
--
module Network.AWS.LexModels.GetUtterancesView
    (
    -- * Creating a Request
      getUtterancesView
    , GetUtterancesView
    -- * Request Lenses
    , guvBotName
    , guvBotVersions
    , guvStatusType

    -- * Destructuring the Response
    , getUtterancesViewResponse
    , GetUtterancesViewResponse
    -- * Response Lenses
    , guvrsBotName
    , guvrsUtterances
    , guvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getUtterancesView' smart constructor.
data GetUtterancesView = GetUtterancesView'
  { _guvBotName     :: !Text
  , _guvBotVersions :: !(List1 Text)
  , _guvStatusType  :: !StatusType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUtterancesView' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guvBotName' - The name of the bot for which utterance information should be returned.
--
-- * 'guvBotVersions' - An array of bot versions for which utterance information should be returned. The limit is 5 versions per request.
--
-- * 'guvStatusType' - To return utterances that were recognized and handled, use@Detected@ . To return utterances that were not recognized, use @Missed@ .
getUtterancesView
    :: Text -- ^ 'guvBotName'
    -> NonEmpty Text -- ^ 'guvBotVersions'
    -> StatusType -- ^ 'guvStatusType'
    -> GetUtterancesView
getUtterancesView pBotName_ pBotVersions_ pStatusType_ =
  GetUtterancesView'
    { _guvBotName = pBotName_
    , _guvBotVersions = _List1 # pBotVersions_
    , _guvStatusType = pStatusType_
    }


-- | The name of the bot for which utterance information should be returned.
guvBotName :: Lens' GetUtterancesView Text
guvBotName = lens _guvBotName (\ s a -> s{_guvBotName = a})

-- | An array of bot versions for which utterance information should be returned. The limit is 5 versions per request.
guvBotVersions :: Lens' GetUtterancesView (NonEmpty Text)
guvBotVersions = lens _guvBotVersions (\ s a -> s{_guvBotVersions = a}) . _List1

-- | To return utterances that were recognized and handled, use@Detected@ . To return utterances that were not recognized, use @Missed@ .
guvStatusType :: Lens' GetUtterancesView StatusType
guvStatusType = lens _guvStatusType (\ s a -> s{_guvStatusType = a})

instance AWSRequest GetUtterancesView where
        type Rs GetUtterancesView = GetUtterancesViewResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetUtterancesViewResponse' <$>
                   (x .?> "botName") <*> (x .?> "utterances" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetUtterancesView where

instance NFData GetUtterancesView where

instance ToHeaders GetUtterancesView where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetUtterancesView where
        toPath GetUtterancesView'{..}
          = mconcat ["/bots/", toBS _guvBotName, "/utterances"]

instance ToQuery GetUtterancesView where
        toQuery GetUtterancesView'{..}
          = mconcat
              ["bot_versions" =:
                 toQueryList "member" _guvBotVersions,
               "status_type" =: _guvStatusType, "view=aggregation"]

-- | /See:/ 'getUtterancesViewResponse' smart constructor.
data GetUtterancesViewResponse = GetUtterancesViewResponse'
  { _guvrsBotName        :: !(Maybe Text)
  , _guvrsUtterances     :: !(Maybe [UtteranceList])
  , _guvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUtterancesViewResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guvrsBotName' - The name of the bot for which utterance information was returned.
--
-- * 'guvrsUtterances' - An array of 'UtteranceList' objects, each containing a list of 'UtteranceData' objects describing the utterances that were processed by your bot. The response contains a maximum of 100 @UtteranceData@ objects for each version.
--
-- * 'guvrsResponseStatus' - -- | The response status code.
getUtterancesViewResponse
    :: Int -- ^ 'guvrsResponseStatus'
    -> GetUtterancesViewResponse
getUtterancesViewResponse pResponseStatus_ =
  GetUtterancesViewResponse'
    { _guvrsBotName = Nothing
    , _guvrsUtterances = Nothing
    , _guvrsResponseStatus = pResponseStatus_
    }


-- | The name of the bot for which utterance information was returned.
guvrsBotName :: Lens' GetUtterancesViewResponse (Maybe Text)
guvrsBotName = lens _guvrsBotName (\ s a -> s{_guvrsBotName = a})

-- | An array of 'UtteranceList' objects, each containing a list of 'UtteranceData' objects describing the utterances that were processed by your bot. The response contains a maximum of 100 @UtteranceData@ objects for each version.
guvrsUtterances :: Lens' GetUtterancesViewResponse [UtteranceList]
guvrsUtterances = lens _guvrsUtterances (\ s a -> s{_guvrsUtterances = a}) . _Default . _Coerce

-- | -- | The response status code.
guvrsResponseStatus :: Lens' GetUtterancesViewResponse Int
guvrsResponseStatus = lens _guvrsResponseStatus (\ s a -> s{_guvrsResponseStatus = a})

instance NFData GetUtterancesViewResponse where
